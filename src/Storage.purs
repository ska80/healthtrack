module Storage where

import Prelude (Unit, bind, discard, pure, unit, ($))
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Data.Either (Either)
import Foreign (MultipleErrors)
import Model
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

store :: AppState -> Aff Unit
store appState =
  let
    serialized = JSON.writeJSON appState
  in
   storeValue appStateKey serialized

retrieve :: Aff (Either MultipleErrors AppState)
retrieve = do
  val <- retrieveValue appStateKey
  pure $ (JSON.readJSON val :: Either MultipleErrors AppState)
