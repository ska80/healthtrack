module AddEntryScreen where

import Prelude

import Data.Array ((:))
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Model (AppState, Screen(..), CreatedAtInst(..), ItemEntry(..))
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
  | SelectEntryType AddEntryScreenType

data AddEntryScreenType
  = ChooseNewEntryType
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
  , initialState: { appState: props.state, currentScreen: ChooseNewEntryType }
  } props
  where
    update self =
      case _ of
        SelectEntryType chosenScreen ->
          Update $ self.state { currentScreen = chosenScreen }

        AddItem ->
          SideEffects doAddItem
            where
              doAddItem :: Self Props AddEntryScreenState -> Effect Unit
              doAddItem self' = do
                now' <- now
                let
                  nextEntry =
                    { key: show self'.state.appState.nextId
                    , val: TextItem $ maybe "" identity self'.state.appState.textVal
                    , createdAt: CreatedAtInst $ UTCInst now'
                    }
                  nextState = self'.state.appState
                    { items =  nextEntry : self'.state.appState.items
                    , nextId = self'.state.appState.nextId + 1
                    , textVal = Nothing
                    }
                self'.setState $ _ { appState = nextState }
                props.onStateUpdate nextState
                props.changeScreen ViewLogScreen

    send = runUpdate update

    render self =
      view { style: css { flexDirection: "column", padding: 50
                        , width: "100%", height: "100%"
                        }
           , children:
             [ button { title: "< Menu"
                      , key: "MenuButton"
                      , onPress: capture_ props.returnToMenuE
                      }
             , button { title: "View Existing Entries"
                      , key: "ViewLogButton"
                      , onPress: capture_ (props.changeScreen ViewLogScreen)
                      }
             , text { key: "instructions"
                    , children: [ string "Choose entry type:" ]
                    }
             , button { title: "Symptom"
                      , key: "SymptomButton"
                      , onPress: capture_ (send self $ SelectEntryType SymptomEntryType)
                      }
             , button { title: "Food"
                      , key: "FoodButton"
                      , onPress: capture_ (send self $ SelectEntryType FoodEntryType)
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
                         , value: maybe "" identity self.state.appState.textVal
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
            self.setState \s-> s { appState = s.appState { textVal = tv } }
