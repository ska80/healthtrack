module AddEntryScreen where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Model (AppState, Screen(..), CreatedAtInst(..))
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (text, string, button, view, textInput)
import Effect.Now (now)
import Util as Util

import HealthTrack.Time (UTCInst(..))

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
    updateParentState state = do
      -- log "logEntryScreen updateParent"
      props.onStateUpdate state

    update self =
      case _ of
        AddItem ->
          SideEffects doAddItem
            where
              doAddItem :: Self Props AppState -> Effect Unit
              doAddItem self' = do
                now' <- now
                let
                  nextEntry =
                    { key: show self'.state.nextId
                    , val: maybe "" identity self'.state.textVal
                    , createdAt: CreatedAtInst $ UTCInst now'
                    }
                  nextState = self'.state
                    { items =  nextEntry : self'.state.items
                    , nextId = self'.state.nextId + 1
                    , textVal = Nothing
                    }
                self'.setState $ const nextState
                updateParentState nextState
                props.changeScreen ViewLogScreen

    send = runUpdate update

    render self =
      view { style: css {flexDirection: "column", padding: 50, width: "100%", height: "100%"}
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
             , textInput { key: "txtinput"
                         , placeholder: "Enter entry text here"
                         , style: css { flex: 1
                                      , borderWidth: 1
                                      , borderColor: "black"
                                      , padding: 5
                                      , width: "100%"
                                      }
                         , onChange: (capture Util.getText setStateText)
                         , value: maybe "" identity self.state.textVal
                         , onSubmitEditing: (capture_ $ send self AddItem )
                           -- TODO maybe reenable autocorrect? seems like there should be a better way to handle
                           -- the weird way the app was re-populating the field. idk.
                         , autoCorrect: false
                         , multiline: true
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
