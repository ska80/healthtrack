module HealthTrack.ItemEntryScreen.Text where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Model (Screen(..), CreatedAtInst(..), ItemEntry(..))
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (text, string, button, view, textInput)
import Effect.Now (now)
import Util as Util


import HealthTrack.Time (UTCInst(..))

comp :: Component Props
comp = createComponent "AddTextEntryScreen"

data Action
  = AddItem

type Props =
  { onEntryComplete :: ItemEntry -> Effect Unit
  , key :: String
  }

type TextEntryScreenState =
  { textVal :: Maybe String
  }

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: { textVal: Nothing }
  } props
  where
    update :: Self Props TextEntryScreenState -> Action -> StateUpdate Props TextEntryScreenState
    update self =
      case _ of
        AddItem ->
          SideEffects doAddItem
            where
              doAddItem :: Self Props TextEntryScreenState -> Effect Unit
              doAddItem self' = do
                now' <- now
                let
                  nextEntry =
                    TextItem $ maybe "" identity self'.state.textVal
                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      view { style: css { flexDirection: "column", padding: 50
                        , width: "100%", height: "100%"
                        }
           , key: self.props.key
           , children:
             [ text { key: "instructions"
                    , children: [ string "Choose entry type:" ]
                    }
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
                           -- TODO maybe reenable autocorrect? seems like there
                           -- should be a better way to fix the weird way the
                           -- app was re-populating the field. idk.
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
