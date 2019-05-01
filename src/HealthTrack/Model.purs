module HealthTrack.Model where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import HealthTrack.Time (UTCInst, TZOffset(..))
import Data.Time.Duration (Minutes(..))
import Data.Maybe (Maybe(..))
import Data.Debug as D

type AppState =
  { nextId :: Int
  , items :: Array Item
  , currentScreen :: Screen
  , userTZOffset :: TZOffset
  }

-- TODO convert state to be a made up of a list of changes (event source-y)
-- TODO add more data here: esp date added, date modified

ppAppStateOpts :: { maxDepth :: Maybe Int
                  , compactThreshold :: Int
                  }
ppAppStateOpts =
  { maxDepth : Nothing -- always print everything, don't elide
  , compactThreshold : 8
  }

ppAppState :: AppState -> String
ppAppState = D.prettyPrintWith ppAppStateOpts <<< D.debug

type Item =
  { key :: String
  , entry :: ItemEntry
  , createdAt :: CreatedAtInst
  }

newtype ItemName = ItemName String

derive instance genericItemName :: Generic ItemName _

instance itemNameShow :: Show ItemName where
  show = genericShow

instance itemNameDebug :: D.Debug ItemName where
  debug = D.genericDebug

data ItemEntry
  -- = FoodItem String
  = FoodItem ItemName
    -- food, water, etc
  | ConditionItem String
    -- stress, sickness, etc
  | SymptomItem String
    -- pain, etc
  | ActivityItem String
    -- exercise, walking, etc
  | NoteItem String
    -- uncategorized, free form note

derive instance genericItemEntry :: Generic ItemEntry _

instance itemEntryShow :: Show ItemEntry where
  show = genericShow

instance debugItemEntry :: D.Debug ItemEntry where
  debug = D.genericDebug

newtype CreatedAtInst = CreatedAtInst UTCInst

derive instance genericCreatedAtInst :: Generic CreatedAtInst _

instance createdAtInstShow :: Show CreatedAtInst where
  show = genericShow

instance debugCreatedAtInst :: D.Debug CreatedAtInst where
  debug = D.genericDebug

data Screen
  = MenuScreen
  | AddItemScreen
  | DeveloperScreen
  | ViewLogScreen

derive instance genericScreen :: Generic Screen _

instance screensShow :: Show Screen where
  show = genericShow

instance debugScreen :: D.Debug Screen where
  debug = D.genericDebug

initialState :: AppState
initialState = { nextId: 0
               , items: []
               , currentScreen : MenuScreen
               , userTZOffset : TZOffset (Minutes 0.0)
               }
