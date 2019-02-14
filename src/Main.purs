module Main where
import Prelude (($), (+), (<>), show)
-- import Effect.Console
-- import Effect
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.Native (text_, text, string, view_, button, view, textInput)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn, merge)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import Unsafe.Coerce (unsafeCoerce)

comp :: Component {}
comp = createComponent "Main"

data Action
  = Increment

type State =
  { count :: Int
  , textVal :: Maybe String
  }
initialVal :: State
initialVal = { count: 0, textVal: Nothing }

-- main :: RB.JSX
-- main = RN.text_ [ RN.string "hey" ]

main :: JSX
main = make comp
  { render, initialState: initialVal } {}
  where
    update self =
      case _ of
        Increment ->
          Update $ self.state { count = self.state.count + 1 }

    send = runUpdate update

    -- render :: Self {} State Action -> JSX
    render self = -- text_ [ string $ "hi"]
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ text {  key: "foo",
                       children: [
                         string $ "hey " <> show self.state.count <> ";" <> show self.state.textVal ]}
             , textInput { key: "txtinput"
                         , placeholder: "type here"
                         , style: css { flex: 1 }
                         , onChange: (capture (merge {getText}) setThing)
                         }
             , button { title: "clickx"
                      , key: "clickyButton"
                      , onPress: (capture_ $ send self Increment)
                      }
             ]
           }
        where
          setThing event =
            self.setState _ { textVal = event.getText }

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
