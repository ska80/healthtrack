module Model where

import Data.List (List(..), (:))

import Data.Maybe (Maybe(..), maybe)

type AppState =
  { nextId :: Int
  , textVal :: Maybe String
  , items :: Array Item
  }

type Item = {key :: String, val :: String }
