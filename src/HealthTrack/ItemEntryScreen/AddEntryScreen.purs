module HealthTrack.ItemEntryScreen.AddEntryScreen where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import HealthTrack.CommonViews (headerButtonsView, wButton)
import HealthTrack.ItemEntryScreen.Food as IEFood
import HealthTrack.ItemEntryScreen.Condition as IECondition
import HealthTrack.ItemEntryScreen.Symptom as IESymptom
import HealthTrack.ItemEntryScreen.Activity as IEActivity
import HealthTrack.ItemEntryScreen.Note as IENote

import HealthTrack.Model (AppState, ItemEntry(..), Screen(..), Item)
import HealthTrack.ModelUtil (addItemEntryToAppState, updateItemEntryInAppState)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (CSS, css)

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
  , item :: Maybe Item
  }

type AddEntryScreenState =
  { currentScreen :: AddEntryScreenType
  , appState :: AppState
  }

styles :: { instructions :: CSS }
styles = { instructions: css { alignSelf: "center"
                             , margin: 20
                             , marginTop: 60
                             }
         }

initialScreen :: AddEntryScreenType
-- initialScreen = NoteEntryType
initialScreen = ChooseNewEntryType

getInitialState :: Props -> AddEntryScreenState
getInitialState props =
  let
    screen =
      case props.item of
        Nothing -> initialScreen
        Just item -> itemToScreen item
  in
   { appState: props.state
   , currentScreen: screen
   }

itemToScreen :: Item -> AddEntryScreenType
itemToScreen item =
  case item.entry of
    FoodItem _      -> FoodEntryType
    ConditionItem _ -> ConditionEntryType
    SymptomItem _   -> SymptomEntryType
    ActivityItem _  -> ActivityEntryType
    NoteItem _      -> NoteEntryType

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: getInitialState props
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

    onEntryUpdate :: Self Props AddEntryScreenState -> Item -> ItemEntry -> Effect Unit
    onEntryUpdate {state, setState} item itemEntry = do
      nextState <- updateItemEntryInAppState state.appState item itemEntry
      setState $ _ { appState = nextState }
      props.onStateUpdate nextState
      props.changeScreen ViewLogScreen


    render self =
      let
        renderer =
            case self.state.currentScreen of
              ChooseNewEntryType -> renderChooseNewEntryType
              FoodEntryType      -> renderFoodEntryType
              ConditionEntryType -> renderConditionEntryType
              SymptomEntryType   -> renderSymptomEntryType
              ActivityEntryType  -> renderActivityEntryType
              NoteEntryType      -> renderNoteEntryType
      in
       renderer self.props.item
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

        renderChooseNewEntryType _item =
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

        renderFoodEntryType item =
          wrapperView children
          where
            children =
              [ IEFood.form { key: "IEFoodElem"
                            , onEntryComplete: onEntryComplete self
                            , items: self.state.appState.items
                            , item
                            }
              ]

        renderConditionEntryType item =
          wrapperView children
          where
            children =
              [ IECondition.form { key: "IEConditionElem"
                                 , onEntryComplete: onEntryComplete self
                                 , items: self.state.appState.items
                                 , item
                                 }
              ]

        renderSymptomEntryType item =
          wrapperView children
          where
            children =
              [ IESymptom.form { key: "IESymptomElem"
                               , onEntryComplete: onEntryComplete self
                               , items: self.state.appState.items
                               , item
                               }
              ]

        renderActivityEntryType item =
          wrapperView children
          where
            children =
              [ IEActivity.form { key: "IEActivityElem"
                                , onEntryComplete: onEntryComplete self
                                  -- TODO add updating for all the other types
                                , onEntryUpdate: onEntryUpdate self
                                , items: self.state.appState.items
                                , item
                                }
              ]

        renderNoteEntryType item =
          wrapperView children
          where
            children =
              [ IENote.form { key: "IENoteElem"
                            , onEntryComplete: onEntryComplete self
                            , item
                            }
              ]
