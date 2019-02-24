module AddEntryScreen where

import Data.Array ((:))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Model (AppState, Screen(..))
import Prelude (Unit, identity, show, ($), (+))
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import React.Basic.Native (text, string, button, view, textInput)
import Unsafe.Coerce (unsafeCoerce)

comp :: Component Props
comp = createComponent "AddEntryScreen"

data Action
  = AddItem

type Props =
  { state :: AppState
  , onStateUpdate :: AppState -> Effect Unit
  , returnToMenuE :: Effect Unit
  , changeScreen :: Screen -> Effect Unit
  }

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: props.state
  } props
  where
    updateParent self = do
      -- log "logEntryScreen updateParent"
      self.props.onStateUpdate self.state

    update self =
      case _ of
        AddItem ->
          let
            nextEntry =
              { key: show self.state.nextId
              , val: maybe "" identity self.state.textVal
              }

            nextState = self.state
              { items =  nextEntry : self.state.items
              , nextId = self.state.nextId + 1
              , textVal = Nothing
              }
          in
           UpdateAndSideEffects nextState updateParent

    send = runUpdate update

    render self =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ button { title: "< Menu"
                      , key: "MenuButton"
                      , onPress: capture_ props.returnToMenuE
                      }
             , button { title: "View Existing Entries"
                      , key: "ViewLogButton"
                      , onPress: capture_ (props.changeScreen ViewLogScreen)
                      }
               -- TODO better instructions?
             , text { key: "instructions", children: [ string "Add a New Entry" ] }

               -- TODO make text input multiline
               -- TODO style so that its easier to tell where the text box is
             , textInput { key: "txtinput"
                         , placeholder: "Enter entry text here"
                         , style: css { flex: 1 }
                         , onChange: (capture getText setStateText)
                         , value: maybe "" identity self.state.textVal
                         , onSubmitEditing: (capture_ $ send self AddItem )
                           -- TODO maybe reenable autocorrect? seems like there should be a better way to handle
                           -- the weird way the app was re-populating the field. idk.
                         , autoCorrect: false
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


-- TODO should probably move this to util
getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
