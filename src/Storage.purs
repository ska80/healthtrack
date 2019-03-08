module Storage where

import Prelude

import Control.Monad.Error.Class

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (MultipleErrors, fail, ForeignError(..))
import Model (AppState, initialState, Screen(..), Item, CreatedAtInst(..))
import Data.DateTime.Instant (Instant(..), instant)
import Data.Time.Duration (Milliseconds(..))
import Simple.JSON as JSON
import Data.Traversable (traverse)
import Global.Unsafe (unsafeStringify)
import Foreign.JSON as FJ

import Control.Monad.Except (runExcept, withExcept)
import Foreign as F
import Foreign.Index as FI
import HealthTrack.Time (UTCInst(..))
import HealthTrack.TimeUtil (getTZOffset)

import Debug.Trace

foreign import storeData_ :: String -> String -> EffectFnAff Unit

foreign import retrieveData_ :: String -> EffectFnAff String

retrieveValue :: String -> Aff String
retrieveValue key = do
  val <- fromEffectFnAff $ retrieveData_ key
  pure val

storeValue :: String -> String -> Aff Unit
storeValue key value = do
  fromEffectFnAff $ storeData_ key value
  pure unit

appStateKey :: String
appStateKey = "AppState"

storeAppState :: AppState -> Aff Unit
storeAppState appState =
  let
    serialized = serializeAppState appState
  in
   storeValue appStateKey serialized

serializeAppState :: AppState -> String
serializeAppState state =
  let serialized =
        {
          items: state.items,
          nextId: state.nextId
        }
  in
   spy "serialized" $ unsafeStringify serialized


-- load and set up anything required to prepare the app state after
-- application start
loadAndInitializeAppState :: Aff (Either MultipleErrors AppState)
loadAndInitializeAppState = do
  tzOffset <- liftEffect $ getTZOffset
  val <- retrieveValue appStateKey
  let
    makeThing :: { items :: Array Item
                 , nextId :: Int } -> AppState
    makeThing parsed =
      { textVal : Nothing
      , nextId : parsed.nextId
      , items: parsed.items
      , currentScreen : MenuScreen
      , userTZOffset : tzOffset
      }
  pure $ (makeThing <$> parseSavedState val)

parseSavedState :: String -> (Either MultipleErrors
                              { items :: Array Item
                              , nextId :: Int })
parseSavedState val =
  runExcept do
    parsed <- FJ.parseJSON val

    nextId <- (FI.readProp "nextId" >=> F.readInt ) parsed

    itemsFA <- (FI.readProp "items" >=> F.readArray) parsed

    items <- readItems itemsFA

    pure  { nextId : nextId
          , items: items
          }

-- initializeLocalTimezoneOffset :: Effect TZOffset

readItems :: Array F.Foreign -> F.F (Array Item)
readItems itemsF =
  traverse readItem itemsF

readItem :: F.Foreign -> F.F Item
readItem itemF = do
  createdAt' <- (FI.readProp "createdAt" >=> F.readNumber ) itemF
  let createdAt'' = instant (Milliseconds createdAt')
  createdAt <- case createdAt'' of
    Just ca -> pure $ CreatedAtInst $ UTCInst ca
    Nothing -> fail $ ForeignError "could not parse createdAt time for item"

  key <- (FI.readProp "key" >=> F.readString ) itemF
  val <- (FI.readProp "val" >=> F.readString ) itemF
  pure { createdAt, key, val }
