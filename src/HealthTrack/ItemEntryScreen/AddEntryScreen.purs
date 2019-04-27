module HealthTrack.ItemEntryScreen.AddEntryScreen where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import HealthTrack.ItemEntryScreen.Food as IEFood
import HealthTrack.ItemEntryScreen.Condition as IECondition
import HealthTrack.ItemEntryScreen.Symptom as IESymptom
import HealthTrack.ItemEntryScreen.Activity as IEActivity
import HealthTrack.ItemEntryScreen.Note as IENote

import HealthTrack.Model (AppState, ItemEntry, Screen(..))
import HealthTrack.ModelUtil (addItemEntryToAppState)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css, CSS)
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (SyntheticEvent)
import React.Basic.Native (text, string, button, view)

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

-- TODO make all the screens that should go "back" to the prev screen go to the "choose"
-- screen do it

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: { appState: props.state
                  -- , currentScreen: NoteEntryType
                  , currentScreen: ChooseNewEntryType
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
          view { style: styles.wrapper
               , key:  "WrapperView2"
               , children:
                 [ view { style: styles.headerButtonsWrap
                        , key: "headerButtonWrapper"
                        , children: [ wButton { title: "< Menu"
                                              , key:  "MenuButton"
                                              , onPress: capture_ props.returnToMenuE
                                              , style: css {}
                                              }
                                    , wButton { title: "Existing Entries"
                                              , key:  "ViewLogButton"
                                              , onPress: capture_ (props.changeScreen ViewLogScreen)
                                              , style: css {}
                                              }
                                    ]
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
                     , style: styles.instructions
                     }
              , wButton { title: "Food"
                        , key: "foodbutton"
                        , onPress: capture_ (send self $ SelectEntryType FoodEntryType)
                        , style: styles.choiceButton
                        }
              , wButton { title: "Condition"
                        , key: "conditionbutton"
                        , onPress: capture_ (send self $ SelectEntryType ConditionEntryType)
                        , style: styles.choiceButton
                        }
              , wButton { title: "Symptom"
                        , key: "symptombutton"
                        , onPress: capture_ (send self $ SelectEntryType SymptomEntryType)
                        , style: styles.choiceButton
                        }
              , wButton { title: "Activity"
                        , key: "activitybutton"
                        , onPress: capture_ (send self $ SelectEntryType ActivityEntryType)
                        , style: styles.choiceButton
                        }
              , wButton { title: "Note"
                        , key: "NoteButton"
                        , onPress: capture_ (send self $ SelectEntryType NoteEntryType)
                        , style: styles.choiceButton
                        }
              ]

        renderFoodEntryType _ignored =
          wrapperView children
          where
            children =
              [ IEFood.form { key: "IEFoodElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               } ]

        renderConditionEntryType _ignored =
          wrapperView children
          where
            children =
              [ IECondition.form { key: "IEConditionElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               } ]

        renderSymptomEntryType _ignored =
          wrapperView children
          where
            children =
              [ IESymptom.form { key: "IESymptomElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               } ]

        renderActivityEntryType _ignored =
          wrapperView children
          where
            children =
              [ IEActivity.form { key: "IEActivityElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               } ]

        renderNoteEntryType _ignored =
          wrapperView children
          where
            children =
              [ IENote.form { key: "IENoteElem"
                            , onEntryComplete: onEntryComplete self
                            }
]

    styles = { wrapper: css { flexDirection: "column"
                            , padding: 50
                            , width: "100%"
                            , height: "100%"
                            }
             , instructions: css { alignSelf: "center"
                                 , margin: 20
                                 , marginTop: 60
                                 }
             , choiceButton: css { margin: 20 }
             , headerButtonsWrap: css { flexDirection: "row"
                                      , justifyContent: "space-between"
                                      }
             }

wButton :: { title :: String, key :: String, onPress :: EffectFn1 SyntheticEvent Unit, style :: CSS } -> JSX
wButton props =
  view { key: props.key
       , style: props.style
       , children: [ button { title: props.title
                            , key: props.key
                            , onPress: props.onPress
                            }
                   ]
       }
