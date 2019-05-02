module HealthTrack.ItemEntryScreen.Note where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import HealthTrack.Model (ItemEntry(..))
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string, button, view)
import Effect.Now (now)
import HealthTrack.CommonViews as CV

comp :: Component Props
comp = createComponent "AddNoteEntryScreen"

data Action
  = AddItem

type Props =
  { onEntryComplete :: ItemEntry -> Effect Unit
  , key :: String
  }

type NoteEntryScreenState =
  { textVal :: Maybe String
  }

form :: Props -> JSX
form props = make comp
  { render
  , initialState: { textVal: Nothing }
  } props
  where
    update :: Self Props NoteEntryScreenState -> Action -> StateUpdate Props NoteEntryScreenState
    update self =
      case _ of
        AddItem ->
          SideEffects doAddItem
            where
              doAddItem :: Self Props NoteEntryScreenState -> Effect Unit
              doAddItem self' = do
                now' <- now
                let
                  nextEntry =
                    NoteItem $ maybe "" identity self'.state.textVal
                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      view { style: css { flexDirection: "column"
                        , padding: 50
                        , width: "100%"
                        , height: 400
                        }
           , key: self.props.key
           , children:
             [ text { key: "instructions"
                    , children: [ string "Note:" ]
                    }
             , CV.notesInput self.state.textVal setStateText (send self AddItem)
             , button { title: "save"
                      , key: "clickyButton"
                      , onPress: (capture_ $ send self AddItem )
                      }
             ]
           }
        where
          setStateText tv =
            self.setState _ { textVal = tv }
