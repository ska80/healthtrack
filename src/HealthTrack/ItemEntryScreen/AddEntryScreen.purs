module HealthTrack.ItemEntryScreen.AddEntryScreen where

import Prelude

import Effect (Effect)
import HealthTrack.ItemEntryScreen.Note as IENote
import HealthTrack.ItemEntryScreen.Symptom as IESymptom
import HealthTrack.Model (AppState, ItemEntry, Screen(..))
import HealthTrack.ModelUtil (addItemEntryToAppState)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string, button, view)

comp :: Component Props
comp = createComponent "AddEntryScreen"

data Action
  = SelectEntryType AddEntryScreenType

data AddEntryScreenType
  = ChooseNewEntryType
  | NoteEntryType
  | SymptomEntryType
  | FoodEntryType

type Props =
  { state :: AppState
  , onStateUpdate :: AppState -> Effect Unit
  , returnToMenuE :: Effect Unit
  , changeScreen :: Screen -> Effect Unit
  }

type AddEntryScreenState =
  { currentScreen :: AddEntryScreenType
  , appState :: AppState
  }

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: { appState: props.state
                    -- TODO dont forget to restore original "choose new entry type" screen
                  , currentScreen: SymptomEntryType -- ChooseNewEntryType
                  }
  } props
  where
    update :: Self Props AddEntryScreenState -> Action
              -> StateUpdate Props AddEntryScreenState
    update self =
      case _ of
        SelectEntryType chosenScreen ->
          Update $ self.state { currentScreen = chosenScreen }

    send = runUpdate update

    onEntryComplete :: Self Props AddEntryScreenState -> ItemEntry -> Effect Unit
    onEntryComplete self' itemEntry = do
      nextState <- addItemEntryToAppState self'.state.appState itemEntry
      self'.setState $ _ { appState = nextState }
      props.onStateUpdate nextState
      props.changeScreen ViewLogScreen

    render self =
      case self.state.currentScreen of
        ChooseNewEntryType -> renderChooseNewEntryType unit
        NoteEntryType -> renderNoteEntryType unit
        SymptomEntryType -> renderSymptomEntryType unit
        _ -> renderChooseNewEntryType unit

      where
        wrapperView children =
         view { style: css { flexDirection: "column", padding: 50
                            , width: "100%", height: "100%"
                            }
               , key:  "WrapperView2"
               , children:
                 [ button { title: "< Menu"
                          , key:  "MenuButton"
                          , onPress: capture_ props.returnToMenuE
                          }
                 , button { title: "View Existing Entries"
                          , key:  "ViewLogButton"
                          , onPress: capture_ (props.changeScreen ViewLogScreen)
                          }
                 , view { key: "wrapperView", children }
                 ]
               }

        renderChooseNewEntryType _ignored =
          wrapperView children
          where
            children =
              [ text { key: "instructionsLabel"
                     , children: [ string "Choose entry type:" ]
                     }
              , button { title: "Note"
                       , key: "NoteButton"
                       , onPress: capture_ (send self $ SelectEntryType NoteEntryType)
                       }
              , button { title: "Symptom"
                       , key: "symptombutton"
                       , onPress: capture_ (send self $ SelectEntryType SymptomEntryType)
                       }
              ]


        renderNoteEntryType _ignored =
          wrapperView children
          where
            children =
              [ IENote.form { key: "IENoteElem"
                            , onEntryComplete: onEntryComplete self
                            } ]

        renderSymptomEntryType _ignored =
          wrapperView children
          where
            children =
              [ IESymptom.form { key: "IESymptomElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               } ]
