module HealthTrack.ItemEntryScreen.AddEntryScreen where

import Prelude

import Effect (Effect)
import HealthTrack.CommonViews (headerButtonsView, wButton)
import HealthTrack.ItemEntryScreen.Food as IEFood
import HealthTrack.ItemEntryScreen.Condition as IECondition
import HealthTrack.ItemEntryScreen.Symptom as IESymptom
import HealthTrack.ItemEntryScreen.Activity as IEActivity
import HealthTrack.ItemEntryScreen.Note as IENote

import HealthTrack.Model (AppState, ItemEntry, Screen(..))
import HealthTrack.ModelUtil (addItemEntryToAppState)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (text, string)

comp :: Component Props
comp = createComponent "AddEntryScreen"

data Action
  = SelectEntryType AddEntryScreenType

data AddEntryScreenType
  = ChooseNewEntryType
  | FoodEntryType
  | ConditionEntryType
  | SymptomEntryType
  | ActivityEntryType
  | NoteEntryType

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


styles = { instructions: css { alignSelf: "center"
                             , margin: 20
                             , marginTop: 60
                             }
         }


-- TODO make all the screens that should go "back" to the prev screen go to the "choose"
-- screen do it

-- initialScreen = NoteEntryType
initialScreen = ChooseNewEntryType

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: { appState: props.state
                  , currentScreen: initialScreen
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
        FoodEntryType -> renderFoodEntryType unit
        ConditionEntryType -> renderConditionEntryType unit
        SymptomEntryType -> renderSymptomEntryType unit
        ActivityEntryType -> renderActivityEntryType unit
        NoteEntryType -> renderNoteEntryType unit

      where
        wrapperView children =
          let
            props' =
              { backButton: { text: "Menu"
                            , action: capture_ props.returnToMenuE
                            }
              , optionButton: { text: "Existing Entries"
                              , action: capture_ (props.changeScreen ViewLogScreen)
                              }
              }
          in
           headerButtonsView props' children

        selectEntryType et =
          capture_ (send self $ SelectEntryType et)

        renderChooseNewEntryType _ignored =
          wrapperView children
          where
            children =
              [ text { key: "instructionsLabel"
                     , children: [ string "Choose entry type:" ]
                     , style: styles.instructions
                     }
              , wButton { title: "Food"
                        , key: "foodbutton"
                        , onPress: selectEntryType FoodEntryType
                        }
              , wButton { title: "Condition"
                        , key: "conditionbutton"
                        , onPress: selectEntryType ConditionEntryType
                        }
              , wButton { title: "Symptom"
                        , key: "symptombutton"
                        , onPress: selectEntryType SymptomEntryType
                        }
              , wButton { title: "Activity"
                        , key: "activitybutton"
                        , onPress: selectEntryType ActivityEntryType
                        }
              , wButton { title: "Note"
                        , key: "NoteButton"
                        , onPress: selectEntryType NoteEntryType
                        }
              ]

        renderFoodEntryType _ignored =
          wrapperView children
          where
            children =
              [ IEFood.form { key: "IEFoodElem"
                            , onEntryComplete: onEntryComplete self
                            , items: self.state.appState.items
                            }
              ]

        renderConditionEntryType _ignored =
          wrapperView children
          where
            children =
              [ IECondition.form { key: "IEConditionElem"
                                 , onEntryComplete: onEntryComplete self
                                 , items: self.state.appState.items
                                 }
              ]

        renderSymptomEntryType _ignored =
          wrapperView children
          where
            children =
              [ IESymptom.form { key: "IESymptomElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               }
              ]

        renderActivityEntryType _ignored =
          wrapperView children
          where
            children =
              [ IEActivity.form { key: "IEActivityElem"
                                , onEntryComplete: onEntryComplete self
                                , items: self.state.appState.items
                                }
              ]

        renderNoteEntryType _ignored =
          wrapperView children
          where
            children =
              [ IENote.form { key: "IENoteElem"
                            , onEntryComplete: onEntryComplete self
                            }
              ]
