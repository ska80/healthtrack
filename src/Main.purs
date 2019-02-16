module Main where
import Prelude (($), (+), (<>), show, identity)
-- import Effect.Console
-- import Effect
import Data.List (List(..), (:))
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), maybe)
-- import Data.Tuple (Tuple(..))
import Data.Nullable (toMaybe)
import React.Basic (Self, StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (text_, text, string, button, view, textInput, flatList)

import Unsafe.Coerce (unsafeCoerce)

comp :: Component {}
comp = createComponent "Main"

data Action
  = AddItem

type State =
  { nextId :: Int
  , textVal :: Maybe String
  , items :: List Item
  }

type Item = {key :: String, val :: String }

initialVal :: State
initialVal = { nextId: 0,
               textVal: Nothing,
               items: Nil }

main :: JSX
main = make comp
  { render, initialState: initialVal } {}
  where
    update self =
      case _ of
        AddItem ->
          let
            nextEntry = { key: show self.state.nextId
                        , val: maybe "" identity self.state.textVal }
          in
           Update $ self.state {
             items =  nextEntry : self.state.items,
             nextId = self.state.nextId + 1,
             textVal = Nothing
             }

    send = runUpdate update

    initialList :: List Item
    initialList = (({key: "1", val: "ye"}) :  ({key: "2", val: "foo"}) : Nil)

    initialArray :: Array Item
    initialArray = fromFoldable initialList
    -- initialArray = [{key: 1, val: "ye"}, {key: 2, val: "foo"}]

    render self =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ debugLabelInfoView self
             , textInput { key: "txtinput"
                         , placeholder: "type here"
                         , style: css { flex: 1 }
                         , onChange: (capture getText setStateText)
                         , value: maybe "" identity self.state.textVal
                         , onSubmitEditing: (capture_ $ send self AddItem )
                         , autoCorrect: false
                         }
             , flatList { data: unsafeCoerce $ fromFoldable self.state.items
                        , key: "itemsList"
                        , renderItem: unsafeCoerce (\{item}-> text { children: [ string item.val ] })
                        }
             , button { title: "save"
                      , key: "clickyButton"
                      , onPress: (capture_ $ send self AddItem )
                      }
             ]
           }
        where
          setStateText tv =
            self.setState _ { textVal = tv }

showDebugInfo :: Boolean
-- showDebugInfo = false
showDebugInfo = true

debugLabelInfoView :: Self {} State -> JSX
debugLabelInfoView self =
  if showDebugInfo then
    let
      debugMsg = "debug: " <> show self.state.nextId <>
                 ";" <> show self.state.textVal <>
                 ";" <> show self.state.items
    in
     text { key: "debugLabelInfo",
            children: [string debugMsg]
          }
  else
    view {key: "debugLabelInfoEmpty", children: []}

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
