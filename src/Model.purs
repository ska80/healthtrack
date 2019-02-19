module Model where

import Data.List (List(..), (:))

import Data.Maybe (Maybe(..), maybe)

type AppState =
  { nextId :: Int
  , textVal :: Maybe String
  , items :: Array Item
  , currentScreen :: Screens
  }

type Item = {key :: String, val :: String }

data Screens
  = MenuScreen
  | AddItemScreen
