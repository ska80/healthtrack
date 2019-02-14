module Main where
import Prelude (($), (+), (<>), show, identity)
-- import Effect.Console
-- import Effect
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import React.Basic (Self, StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.Native (text_, text, string, view_, button, view, textInput)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn, merge)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import Unsafe.Coerce (unsafeCoerce)

comp :: Component {}
comp = createComponent "Main"

data Action
  = AddItem

type State =
  { nextId :: Int
  , textVal :: Maybe String
  , items :: List String
  }

initialVal :: State
initialVal = { nextId: 0,
               textVal: Nothing,
               items: Nil }

-- main :: RB.JSX
-- main = RN.text_ [ RN.string "hey" ]

main :: JSX
main = make comp
  { render, initialState: initialVal } {}
  where
    update self =
      case _ of
        AddItem ->
          let
            nextEntry :: String
            nextEntry = show self.state.nextId <> ": " <> maybe "" identity self.state.textVal
          in
           Update $ self.state {
             items =  nextEntry : self.state.items,
             nextId = self.state.nextId + 1,
             textVal = Nothing
             }

    send = runUpdate update

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
                         }
             , button { title: "clickx"
                      , key: "clickyButton"
                      , onPress: (capture_ $ send self AddItem )
                      }
             ]
           }
        where
          setStateText event =
            self.setState _ { textVal = event}

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
     text {  key: "debugLabelInfo",
             children: [string debugMsg]
          }
  else
    view {key: "debugLabelInfoEmpty", children: []}

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
