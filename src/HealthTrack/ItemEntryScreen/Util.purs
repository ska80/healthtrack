module HealthTrack.ItemEntryScreen.Util where

import Prelude

import HealthTrack.Model (Item, ItemEntry)
import HealthTrack.ModelUtil as MU
import Data.Maybe (Maybe)


type Props a =
  { key :: String
  , items :: Array Item
  , item :: Maybe Item
  | a }

type State =
  { itemName :: Maybe String
  , note :: Maybe String
  , changeName :: Boolean
  }

-- TODO make sure this gets used
initialState :: forall a . Props a -> State
initialState props =
  let
    maybeItemEntry :: Maybe ItemEntry
    maybeItemEntry = props.item <#> _.entry
    itemName = maybeItemEntry >>= MU.itemEntryName
    note :: Maybe String
    note = (maybeItemEntry <#> MU.itemEntryNote)
  in
   { itemName
   , note
   , changeName : false
   }
