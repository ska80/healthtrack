module HealthTrack.ViewLogScreen where

-- TODO rename to somethiong better -- like a common rest thing
-- `ListItemsScreen` etc

import Prelude

import Data.Array (fromFoldable)
import Effect (Effect)
import HealthTrack.CommonViews as CV
import HealthTrack.Model (AppState, Screen(..), Item, ItemName(..), CreatedAtInst(..), ItemEntry(..))
import HealthTrack.ModelUtil as MU
import HealthTrack.TimeUtil as TimeUtil
import React.Basic (JSX, Component, StateUpdate(..), make, runUpdate, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string, button, view, flatList,  FlatListPropsItemSeparatorComponent)
import Unsafe.Coerce (unsafeCoerce)
import HealthTrack.Util (toListRenderItem)

comp :: Component Props
comp = createComponent "ViewLogScreen"

data Action
  = DeleteEntry Item

type Props =
  { returnToMenuE :: Effect Unit
  , state :: AppState
  , changeScreen :: Screen -> Effect Unit
  , onStateUpdate :: AppState -> Effect Unit
  }

type State =
  { appState :: AppState }

viewLogScreen :: Props -> JSX
viewLogScreen props = make comp
   { render
   , initialState: { appState: props.state }
   }
   props
  where
    update :: Self Props State -> Action -> StateUpdate Props State
    update self =
      case _ of
        DeleteEntry entry ->
          let
            newAppState :: AppState
            newAppState = MU.removeItem self.state.appState entry

            newState :: State
            newState = self.state { appState = newAppState }

            doDelete :: Self Props State -> Effect Unit
            doDelete self' =
               self'.props.onStateUpdate newAppState
          in
           UpdateAndSideEffects newState doDelete

    send = runUpdate update

    render self =
      view { style: css {flexDirection: "column", paddingTop: 90, padding: 50, width: "100%" }
           , children:
             [ CV.returnToMenuButton self.props
             , button { title: "Add New Entry"
                      , key: "AddItemScreenButton"
                      , onPress: capture_ (self.props.changeScreen AddItemScreen)
                      }
             , flatList { data: unsafeCoerce $ fromFoldable self.state.appState.items
                        , key: "itemsList"
                        , renderItem: toListRenderItem $ renderItem self send
                        , "ItemSeparatorComponent": toFlatListPropsItemSeparatorComponent separator
                        }
             ]
           }

toFlatListPropsItemSeparatorComponent :: ({ highlighted :: Boolean } -> JSX) -> FlatListPropsItemSeparatorComponent
toFlatListPropsItemSeparatorComponent = unsafeCoerce

separator :: ({ highlighted :: Boolean } -> JSX)
separator {highlighted} =
  view { style: css { borderWidth: 1, backgroundColor: "black", margin: 10 } }

-- TODO add an "edit" button in here somehow
-- TODO add a delete button also
renderItem :: Self Props State -> (Self Props State -> Action -> Effect Unit) -> { item :: Item } -> JSX
renderItem self send {item} =
  let
    offset = self.state.appState.userTZOffset
    (CreatedAtInst utcInst) = item.createdAt
    createdAtFormatted = TimeUtil.utcInstDisplayLocal offset utcInst
    viewChildren =
      [ dispEntryItem item.entry
      , text { key:"createdAt"
             , children: [ string createdAtFormatted ] }
      , button { title: "delete"
               , key: "DeleteButton"
               , onPress: (capture_ $ send self $ DeleteEntry item)
               }
      ]
  in
   view { style: css { flex: 1, flexDirection: "column" }
        , key: item.key
        , children: viewChildren
        }

dispEntryItem :: ItemEntry -> JSX
dispEntryItem val =
  case val of
    FoodItem (ItemName theVal) ->
      text { key: "val", children: [ string $ "Food: " <> theVal ] }
    ConditionItem theVal ->
      text { key: "val", children: [ string $ "Condition: " <> theVal ] }
    SymptomItem theVal ->
      text { key: "val", children: [ string $ "Symptom: " <> theVal ] }
    ActivityItem theVal ->
      text { key: "val", children: [ string $ "Activity: " <> theVal ] }
    NoteItem theVal ->
      text { key: "val", children: [ string  $ "Note: " <> theVal ] }
