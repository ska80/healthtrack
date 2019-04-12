module HealthTrack.ModelUtil where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Effect.Now (now)
import HealthTrack.Model (AppState, Item, ItemEntry(..), CreatedAtInst(..))
import HealthTrack.Time (UTCInst(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))


makeItem :: Int -> ItemEntry -> Effect Item
makeItem id itemEntry = do
  now' <- now
  pure { key: show id
       , entry: itemEntry
       , createdAt: CreatedAtInst $ UTCInst now'
       }

addItemEntryToAppState :: AppState -> ItemEntry -> Effect AppState
addItemEntryToAppState appState itemEntry = do
  newItem <- makeItem appState.nextId itemEntry
  pure $ appState { items = newItem : appState.items
                  , nextId = appState.nextId + 1
                  }

removeItem :: AppState -> Item -> AppState
removeItem appState item =
  let
    isItem item' = item.key /= item'.key
    items' = Array.filter isItem appState.items
  in
   appState { items = items' }

foodItemEntryDescription :: ItemEntry -> Maybe String
foodItemEntryDescription =
  case _ of
    SymptomItem s -> Just s
    _ -> Nothing

foodItemEntryDescriptions :: Array ItemEntry -> Array String
foodItemEntryDescriptions =
  Array.mapMaybe foodItemEntryDescription


symptomItemEntryDescription :: ItemEntry -> Maybe String
symptomItemEntryDescription =
  case _ of
    SymptomItem s -> Just s
    _ -> Nothing

symptomItemEntryDescriptions :: Array ItemEntry -> Array String
symptomItemEntryDescriptions =
  Array.mapMaybe symptomItemEntryDescription
