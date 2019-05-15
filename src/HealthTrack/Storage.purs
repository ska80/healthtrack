module HealthTrack.Storage where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (Foreign, MultipleErrors, fail, ForeignError(..), unsafeToForeign)
import HealthTrack.Model (AppState, Screen(..), Item, CreatedAtInst(..), ItemEntry(..), ItemName(..), ItemNotes(..))
import Data.DateTime.Instant (instant)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Global.Unsafe (unsafeStringify)
import Foreign.JSON as FJ
import Control.Monad.Except (runExcept)
import Foreign as F
import Foreign.Index as FI
-- import Foreign.NullOrUndefined (null)
import HealthTrack.Time (UTCInst(..))
import HealthTrack.TimeUtil (getTZOffset)

foreign import storeData_ :: String -> String -> EffectFnAff Unit

foreign import retrieveData_ :: String -> EffectFnAff String

retrieveValue :: String -> Aff String
retrieveValue key = do
  val <- fromEffectFnAff $ retrieveData_ key
  liftEffect $ log $ "retrieved: " <> val
  pure val

storeValue :: String -> String -> Aff Unit
storeValue key value = do
  fromEffectFnAff $ storeData_ key value
  liftEffect $ log $ "stored: " <> value
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
  let
    serialized =
      { items: serializeItem <$> state.items
      , nextId: state.nextId
      }
  in
   unsafeStringify serialized

-- TODO this is very much garbage, i need to figure out how to do this better
-- I remember there being a thing in simple json about how to handle sum types w/ data
serializeItem :: Item -> Foreign
serializeItem item =
  let
    serializeItemEntry :: ItemEntry -> Foreign
    serializeItemEntry ie =
      case ie of
        FoodItem { name: ItemName name', notes: ItemNotes notes' } ->
          unsafeToForeign
            { _type: "FoodItem"
            , desc: name'
            , notes: notes'
            }
        ConditionItem { name: ItemName name' } ->
          unsafeToForeign
            { _type: "ConditionItem"
            , desc: name'
            }
        SymptomItem { name: ItemName name' } ->
          unsafeToForeign
            { _type: "SymptomItem"
            , desc: name'
            }
        ActivityItem { name: ItemName name' } ->
          unsafeToForeign
            { _type: "ActivityItem"
            , desc: name'
            }
        NoteItem { notes: ItemNotes note' } ->
          unsafeToForeign
            { _type: "NoteItem"
            , desc: unsafeToForeign note'
            }
  in
   unsafeToForeign
     { createdAt:  item.createdAt
     , key: item.key
     , entry: serializeItemEntry item.entry
     }

-- TODO def need to add some kind of tests for serialization/deserialization
-- might also be a good case for some kind of quickcheck like thing
-- generate states, serialize/deserialize, assert they are right

-- load and set up anything required to prepare the app state after
-- application start
loadAndInitializeAppState :: Aff (Either MultipleErrors AppState)
loadAndInitializeAppState = do
  tzOffset <- liftEffect $ getTZOffset
  val <- retrieveValue appStateKey
  let
    currentScreen =  MenuScreen
    -- currentScreen = AddItemScreen

    makeThing :: { items :: Array Item
                 , nextId :: Int } -> AppState
    makeThing parsed =
      { nextId : parsed.nextId
      , items: parsed.items
      , currentScreen : currentScreen
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

    itemsF <- (FI.readProp "items" >=> F.readArray) parsed

    items <- readItems itemsF

    pure  { nextId : nextId
          , items: items
          }

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

  key <- (FI.readProp "key" >=> F.readString) itemF

  itemEntryF <- FI.readProp "entry" itemF

  entry <- readItemEntry itemEntryF
  pure { createdAt, key, entry }

readItemEntry :: F.Foreign -> F.F ItemEntry
readItemEntry itemEntryF = do
  _type <- (FI.readProp "_type" >=> F.readString) itemEntryF
  case _type of
    "FoodItem" -> readFoodItem itemEntryF
    "ConditionItem" -> readConditionItem itemEntryF
    "SymptomItem" -> readSymptomItem itemEntryF
    "ActivityItem" -> readActivityItem itemEntryF
    "NoteItem" -> readNoteItem itemEntryF
    _ -> readNoteItem itemEntryF


readItemSimple :: (String -> ItemEntry) -> F.Foreign -> F.F ItemEntry
readItemSimple ctor itemEntryF = do
  txt <- (FI.readProp "desc" >=> F.readString) itemEntryF
  pure $ ctor txt

readFoodItem :: F.Foreign -> F.F ItemEntry
readFoodItem itemEntryF = do
  txt <- (FI.readProp "desc" >=> F.readString) itemEntryF
  notes <- (FI.readProp "notes" >=> F.readString) itemEntryF
  pure $ FoodItem { name: ItemName txt, notes: ItemNotes notes }

readConditionItem :: F.Foreign -> F.F ItemEntry
readConditionItem = readItemSimple (\s -> ConditionItem { name: ItemName s })

readSymptomItem :: F.Foreign -> F.F ItemEntry
readSymptomItem = readItemSimple (\s -> SymptomItem { name: ItemName s })

readActivityItem :: F.Foreign -> F.F ItemEntry
readActivityItem = readItemSimple (\s -> ActivityItem { name: ItemName s })

readNoteItem :: F.Foreign -> F.F ItemEntry
readNoteItem = readItemSimple (\s -> NoteItem { notes: ItemNotes s })
