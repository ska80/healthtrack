module HealthTrack.ModelUtil where

import Prelude

import Effect (Effect)
import Effect.Now (now)
import HealthTrack.Model (AppState, Item, ItemEntry, Screen(..), CreatedAtInst(..))
import HealthTrack.Time (UTCInst(..))
import Data.Array ((:))
import Data.Array as Array

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
