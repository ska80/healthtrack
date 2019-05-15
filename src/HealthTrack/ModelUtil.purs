module HealthTrack.ModelUtil where

import Prelude

import Data.Array ((:))
import Effect (Effect)
import Effect.Now (now)
import HealthTrack.Model (AppState, Item(..), ItemEntry(..), CreatedAtInst(..), ItemName(..))
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
    ConditionItem s -> Just s
    _ -> Nothing

conditionItemEntryDescriptions :: Array ItemEntry -> Array String
conditionItemEntryDescriptions =
  Array.mapMaybe conditionItemEntryDescription

symptomItemEntryDescription :: ItemEntry -> Maybe String
symptomItemEntryDescription =
  case _ of
    SymptomItem s -> Just s
    _ -> Nothing

symptomItemEntryDescriptions :: Array ItemEntry -> Array String
symptomItemEntryDescriptions =
  Array.mapMaybe symptomItemEntryDescription

activityItemEntryDescription :: ItemEntry -> Maybe String
activityItemEntryDescription =
  case _ of
    ActivityItem s -> Just s
    _ -> Nothing

activityItemEntryDescriptions :: Array ItemEntry -> Array String
activityItemEntryDescriptions =
  Array.mapMaybe activityItemEntryDescription


-- TODO change all items but FooDItem to use ItemName (not NoteItem, should use ItemNote)
-- TODO convert ItemEntry to having data in record, hence have each itemData be its own separate type, can pass just the specific ItemEntry in that case
itemEntryName :: ItemEntry -> Maybe String
itemEntryName = case _ of
  FoodItem { name: ItemName name } -> Just name
  ConditionItem name       -> Just name
  SymptomItem name         -> Just name
  ActivityItem name        -> Just name
  NoteItem _               -> Nothing
