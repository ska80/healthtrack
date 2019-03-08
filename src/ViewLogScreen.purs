module ViewLogScreen where

import Prelude

import CommonViews as CV
import Data.Array (fromFoldable)
import Effect (Effect)
import Model (AppState, Screen(..), Item, CreatedAtInst(..))
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string, button, view, flatList, ListRenderItem, FlatListPropsItemSeparatorComponent)
import Unsafe.Coerce (unsafeCoerce)
import HealthTrack.Time (TZOffset)
import HealthTrack.TimeUtil as TimeUtil

comp :: Component Props
comp = createComponent "ViewLogScreen"

type Props =
  { returnToMenuE :: Effect Unit
  , state :: AppState
  , changeScreen :: Screen -> Effect Unit
  }

-- TODO finish add "new entry" button
viewLogScreen :: Props -> JSX
viewLogScreen props' = makeStateless comp render props'
  where
    render :: Props -> JSX
    render props =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ CV.returnToMenuButton props
             , button { title: "Add New Entry"
                      , key: "AddItemScreenButton"
                      , onPress: capture_ (props.changeScreen AddItemScreen)
                      }

               -- TODO add some separation between items in the list
             , flatList { data: unsafeCoerce $ fromFoldable props.state.items
                        , key: "itemsList"
                        , renderItem: toListRenderItem $ renderItem props.state.userTZOffset
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

renderItem :: TZOffset -> { item :: Item } -> JSX
renderItem offset {item} =
  let
    (CreatedAtInst utcInst) = item.createdAt
    -- TODO make display date time only accept UTCInsts
    createdAtFormatted = TimeUtil.utcInstDisplayLocal offset utcInst
    viewChildren =
      [ text { key: "val"
             , children: [ string item.val ] }
      , text { key:"createdAt"
             , children: [ string createdAtFormatted ] }
      ]
  in
   view { style: css { flex: 1, flexDirection: "column" }
        , key: item.key
        , children: viewChildren
        }
