module ViewLogScreen where

import Prelude

import CommonViews as CV
import Data.Array (fromFoldable)
import Effect (Effect)
import HealthTrack.TimeUtil as TimeUtil
import Model (AppState, Screen(..), Item, CreatedAtInst(..), ItemEntry(..))
import React.Basic (JSX, Component, StateUpdate(..), make, runUpdate, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string, button, view, flatList, ListRenderItem, FlatListPropsItemSeparatorComponent)
import Unsafe.Coerce (unsafeCoerce)

comp :: Component Props
comp = createComponent "ViewLogScreen"

data Action
  = DeleteEntry

type Props =
  { returnToMenuE :: Effect Unit
  , state :: AppState
  , changeScreen :: Screen -> Effect Unit
  }

viewLogScreen :: Props -> JSX
viewLogScreen props = make comp
   { render
   , initialState: props.state
   }
   props
  where
    update self =
      case _ of
        DeleteEntry ->
          NoUpdate

    send = runUpdate update

    render self =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ CV.returnToMenuButton self.props
             , button { title: "Add New Entry"
                      , key: "AddItemScreenButton"
                      , onPress: capture_ (self.props.changeScreen AddItemScreen)
                      }
             , flatList { data: unsafeCoerce $ fromFoldable self.state.items
                        , key: "itemsList"
                        , renderItem: toListRenderItem $ renderItem self send
                        , "ItemSeparatorComponent": toFlatListPropsItemSeparatorComponent separator
                        }
             ]
           }

toListRenderItem :: ({ item :: Item } -> JSX) -> ListRenderItem
toListRenderItem = unsafeCoerce

toFlatListPropsItemSeparatorComponent :: ({ highlighted :: Boolean } -> JSX) -> FlatListPropsItemSeparatorComponent
toFlatListPropsItemSeparatorComponent = unsafeCoerce

separator :: ({ highlighted :: Boolean } -> JSX)
separator {highlighted} =
  view { style: css { borderWidth: 1, backgroundColor: "black", margin: 10 } }


-- TODO add an "edit" button in here somehow
-- TODO add a delete button also
renderItem :: Self Props AppState -> (Self Props AppState -> Action -> Effect Unit) -> { item :: Item } -> JSX
renderItem self send {item} =
  let
    offset = self.state.userTZOffset
    (CreatedAtInst utcInst) = item.createdAt
    createdAtFormatted = TimeUtil.utcInstDisplayLocal offset utcInst
    viewChildren =
      [ text { key: "val"
             , children: [ string item.val ] }
      , text { key:"createdAt"
             , children: [ string createdAtFormatted ] }
      , button { title: "delete"
               , key: "DeleteButton"
               , onPress: (capture_ $ send self DeleteEntry )
               }
      ]
  in
   view { style: css { flex: 1, flexDirection: "column" }
        , key: item.key
        , children: viewChildren
        }
