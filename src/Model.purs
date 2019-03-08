module Model where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (throwError)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Foreign (Foreign, unsafeToForeign, readNumber)
import Foreign as Foreign
import Simple.JSON as JSON
import HealthTrack.Time (UTCInst, TZOffset(..))
import Data.Time.Duration (Minutes(..))

type AppState =
  { nextId :: Int
  , textVal :: Maybe String
  , items :: Array Item
  , currentScreen :: Screen
  , userTZOffset :: TZOffset
  }

-- TODO convert state to be a made up of a list of changes (event source-y)
-- TODO add more data here: esp date added, date modified

type Item =
  { key :: String
  , val :: String
  , createdAt :: CreatedAtInst
  }

newtype CreatedAtInst = CreatedAtInst UTCInst

derive instance genericCreatedAtInst :: Generic CreatedAtInst _

instance createdAtInstShow :: Show CreatedAtInst where
  show = genericShow

data Screen
  = MenuScreen
  | AddItemScreen
  | DeveloperScreen
  | ViewLogScreen

derive instance genericScreen :: Generic Screen _

instance screensShow :: Show Screen where
  show = genericShow

initialState :: AppState
initialState = { nextId: 0
               , textVal: Nothing
               , items: []
               , currentScreen : MenuScreen
               , userTZOffset : TZOffset (Minutes 0.0)
               }
