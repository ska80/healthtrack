module HealthTrack.ViewLogScreen where

-- TODO rename to somethiong better -- like a common rest thing
-- `ListItemsScreen` etc

import Prelude

import Data.Array (fromFoldable)
import Effect (Effect)
import HealthTrack.CommonViews (wButton, headerRowView)
import HealthTrack.Model (AppState, Screen(..), Item, ItemName(..), CreatedAtInst(..), ItemEntry(..), ItemNotes(..))
import HealthTrack.ModelUtil as MU
import HealthTrack.TimeUtil as TimeUtil
import React.Basic (JSX, Component, StateUpdate(..), make, runUpdate, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string, button, view, flatList)
import Unsafe.Coerce (unsafeCoerce)
import HealthTrack.Util (toListRenderItem, toFlatListPropsItemSeparatorComponent)

comp :: Component Props
comp = createComponent "ViewLogScreen"

data Action
  = DeleteEntry Item
  | ToggleEditing

type Props =
  { returnToMenuE :: Effect Unit
  , state :: AppState
  , changeScreen :: Screen -> Effect Unit
  , onStateUpdate :: AppState -> Effect Unit
  }

type State =
  { appState :: AppState
  , isEditing :: Boolean
  }

viewLogScreen :: Props -> JSX
viewLogScreen props = make comp
   { render
   , initialState: { appState: props.state
                   , isEditing: false
                   }
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

        ToggleEditing ->
          Update $ self.state { isEditing = not self.state.isEditing }

    send = runUpdate update
    render self =
      let
        header =
          [ wButton { title: "< Menu"
                    , key:  "MenuButton"
                    , onPress: capture_ self.props.returnToMenuE
                    }
          , wButton { title: "+"
                    , key:  "AddEntryButton"
                    , onPress: capture_ (self.props.changeScreen AddItemScreen)
                    }
          , wButton { title: "Edit"
                    , key:  "EditEntryButton"
                    , onPress: (capture_ $ send self $ ToggleEditing )
                    }
          ]

        body =
          [ flatList { data: unsafeCoerce $ fromFoldable self.state.appState.items
                     , key: "itemsList"
                     , renderItem: toListRenderItem $ renderItem self send
                     , "ItemSeparatorComponent": toFlatListPropsItemSeparatorComponent separator
                     }
          ]
      in
       headerRowView header body


separator :: ({ highlighted :: Boolean } -> JSX)
separator {highlighted} =
  view { style: css { borderWidth: 1, backgroundColor: "black", margin: 10 } }

-- TODO implement edit screens for each entry type
renderItem :: Self Props State -> (Self Props State -> Action -> Effect Unit) -> { item :: Item } -> JSX
renderItem self send {item} =
  let
    offset = self.state.appState.userTZOffset
    (CreatedAtInst utcInst) = item.createdAt
    createdAtFormatted = TimeUtil.utcInstDisplayLocal offset utcInst
    deleteButton = button { title: "delete"
                          , key: "DeleteButton"
                          , onPress: (capture_ $ send self $ DeleteEntry item)
                          }
    viewChildren =
      [ dispEntryItem item.entry
      , text { key:"createdAt"
             , children: [ string createdAtFormatted ] }
      ] <> if self.state.isEditing then [deleteButton] else []
  in
   view { style: css { flex: 1, flexDirection: "column" }
        , key: item.key
        , children: viewChildren
        }

dispEntryItem :: ItemEntry -> JSX
dispEntryItem val =
  case val of
    FoodItem (ItemName name) (ItemNotes notes) ->
      view { key: "foodItem"
           , children: [ text { key: "name"
                              , children: [ string $ "Food: " <> name ]
                              }
                       , text { key: "notes"
                              , children: [ string $ "Notes: " <> notes ]
                              }
                       ]
           }
    ConditionItem theVal ->
      text { key: "val", children: [ string $ "Condition: " <> theVal ] }
    SymptomItem theVal ->
      text { key: "val", children: [ string $ "Symptom: " <> theVal ] }
    ActivityItem theVal ->
      text { key: "val", children: [ string $ "Activity: " <> theVal ] }
    NoteItem theVal ->
      text { key: "val", children: [ string  $ "Note: " <> theVal ] }
