module Storage where

import Data.Either (Either)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign (MultipleErrors)
import Model
import Prelude (Unit, bind, discard, pure, unit, ($))
import Simple.JSON as JSON

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
    serialized = JSON.writeJSON appState
  in
   storeValue appStateKey serialized

retrieveAppState :: Aff (Either MultipleErrors AppState)
retrieveAppState = do
  val <- retrieveValue appStateKey
  pure $ (JSON.readJSON val :: Either MultipleErrors AppState)
