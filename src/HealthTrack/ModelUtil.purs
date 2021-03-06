module HealthTrack.ModelUtil where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Effect.Now (now)
import HealthTrack.Model (AppState, Item, ItemEntry(..), CreatedAtInst(..), ItemName(..), ItemNote(..))
import HealthTrack.Time (UTCInst(..))
import Data.Array as Array
import Data.Maybe (Maybe(..))
import HealthTrack.Util (bool)

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

updateItemEntryInAppState :: AppState -> Item -> ItemEntry -> Effect AppState
updateItemEntryInAppState appState item itemEntry = do
  let newItem = item { entry = itemEntry }
  let replaceOldWithNew oItem =
        bool oItem newItem (oItem.key == newItem.key)
  pure appState { items = replaceOldWithNew <$> appState.items }

removeItem :: AppState -> Item -> AppState
removeItem appState item =
  let
    isItem item' = item.key /= item'.key
    items' = Array.filter isItem appState.items
  in
   appState { items = items' }

foodItemEntryDescription :: ItemEntry -> Maybe ItemName
foodItemEntryDescription =
  case _ of
    FoodItem { name } -> Just name
    _ -> Nothing

foodItemEntryDescriptions :: Array ItemEntry -> Array ItemName
foodItemEntryDescriptions =
  Array.mapMaybe foodItemEntryDescription

conditionItemEntryDescription :: ItemEntry -> Maybe String
conditionItemEntryDescription =
  case _ of
    ConditionItem { name: ItemName name } -> Just name
    _ -> Nothing

conditionItemEntryDescriptions :: Array ItemEntry -> Array String
conditionItemEntryDescriptions =
  Array.mapMaybe conditionItemEntryDescription

symptomItemEntryDescription :: ItemEntry -> Maybe String
symptomItemEntryDescription =
  case _ of
    SymptomItem { name: ItemName name } -> Just name
    _ -> Nothing

symptomItemEntryDescriptions :: Array ItemEntry -> Array String
symptomItemEntryDescriptions =
  Array.mapMaybe symptomItemEntryDescription

activityItemEntryDescription :: ItemEntry -> Maybe String
activityItemEntryDescription =
  case _ of
    ActivityItem { name: ItemName name } -> Just name
    _ -> Nothing

activityItemEntryDescriptions :: Array ItemEntry -> Array String
activityItemEntryDescriptions =
  Array.mapMaybe activityItemEntryDescription

itemEntryName :: ItemEntry -> Maybe String
itemEntryName = case _ of
  FoodItem { name: ItemName name } -> Just name
  ConditionItem { name: ItemName name } -> Just name
  SymptomItem { name: ItemName name } -> Just name
  ActivityItem { name: ItemName name } -> Just name
  NoteItem _               -> Nothing

itemEntryNote :: ItemEntry -> String
itemEntryNote = case _ of
  FoodItem      { note: ItemNote note } -> note
  ConditionItem { note: ItemNote note } -> note
  SymptomItem   { note: ItemNote note } -> note
  ActivityItem  { note: ItemNote note } -> note
  NoteItem      { note: ItemNote note } -> note
