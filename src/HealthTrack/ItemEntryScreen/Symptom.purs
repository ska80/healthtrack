module HealthTrack.ItemEntryScreen.Symptom where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import HealthTrack.Model (ItemEntry(..), Item)
import HealthTrack.Util as Util
import HealthTrack.ModelUtil as MU
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, string, text, view)
import Effect.Now (now)
import Data.String as Str
import Data.List as List
import Data.List (List)
import Data.Array as Array
import HealthTrack.AutoComplete as AC

comp :: Component Props
comp = createComponent "AddSymptomEntryScreen"

data Action
  = SaveItem
  | TextSelected AC.Entry

type Props =
  { onEntryComplete :: ItemEntry -> Effect Unit
  , key :: String
  , items :: Array Item
  }

type State =
  { textVal :: Maybe String
  , selectedText :: Maybe AC.Entry
  }

form :: Props -> JSX
form props = make comp
  { render
  , initialState: { textVal: Nothing, selectedText: Nothing }
  } props
  where
    update :: Self Props State -> Action -> StateUpdate Props State
    update self =
      case _ of
        TextSelected entry ->
          let
            nextState = self.state { selectedText = Just entry}
          in
           Update nextState

        SaveItem ->
          SideEffects doSaveItem
            where
              doSaveItem :: Self Props State -> Effect Unit
              doSaveItem self' = do
                now' <- now
                let
                  nextEntry =
                    SymptomItem $ maybe "" _.val self'.state.selectedText
                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      let
        renderItem {item} = text { children : [ string "string" ] }
        autoComp =
          view { key: "typeAutocomplete"
            , style: css { height: "100%"
                         , width: "100%"
                         }
            , children: [ AC.autoComplete { onItemSelected: itemSelected
                                          , key: "foo"
                                          , initialEntries: symptoms'
                                          , addCreateEntry: true
                                          , handler: inputChangeHandler
                                          }
                        ]
            }

        symptoms' = symptomSuggestions self.props.items

        symptomTypeText {val}=
          text { key: "symptom label"
               , children: [ string $ val]
               }

        itemSelected entry =
          send self $ TextSelected entry

        setStateText tv =
          self.setState _ { textVal = tv }

        inputChangeHandler mtext entries nextId =
          pure $ AC.Response (maybe symptoms' filterEntries mtext) nextId
          where
            filterEntries :: String -> List AC.Entry
            filterEntries str =
              List.filter (hasStr str) symptoms'

            hasStr :: String -> AC.Entry -> Boolean
            hasStr str =
              let
                entryPattern = Str.Pattern (Str.toLower str)
              in
               Str.contains entryPattern <<< Str.toLower <<< _.val
      in
       view { style: css { flexDirection: "column"
                         , padding: 50
                         , width: "100%"
                         , height: "100%"
                         }
            , key: self.props.key
            , children:
              [ text { key: "instructions"
                     , children: [ string "Symptom type:" ]
                     }
              , maybe autoComp symptomTypeText self.state.selectedText
              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: (capture_ $ send self SaveItem )
                       }
              ]}

providedSymptoms :: Array String
providedSymptoms =
  [ "headache"
  , "stomach pain"
  , "joint pain - back"
  , "joint pain - hands"
  , "joint pain - neck"
  ]

symptomSuggestions :: Array Item -> List AC.Entry
symptomSuggestions items =
  let
    -- pull names from item entries for suggestions
    suggestions =
      items <#>
      _.entry #
      MU.foodItemEntryDescriptions #
      Util.filterEmptyStrings #
      Array.nubEq

    allSuggestions = suggestions <> providedSymptoms
  in
   AC.arrayToEntries allSuggestions
