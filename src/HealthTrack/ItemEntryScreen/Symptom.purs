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
  { description :: Maybe String
  , selectedText :: Maybe AC.Entry
  }

form :: Props -> JSX
form props = make comp
  { render
  , initialState: { description: Nothing, selectedText: Nothing }
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
                let
                  nextEntry =
                    SymptomItem $ maybe "" _.val self'.state.selectedText
                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      let
        renderItem {item} = text { children : [ string "string" ] }

        descriptionAutoComplete =
          AC.autoComplete { onItemSelected: itemSelected
                          , key: "descriptionAutoComplete"
                          , initialEntries: allSymptomSuggestions
                          , addCreateEntry: true
                          , handler: inputChangeHandler allSymptomSuggestions
                          }

        autoCompWrapped =
          view { key: "DescriptionWrapper"
               , style: css { height: "100%"
                            , width: "100%"
                            }
               , children: [ descriptionAutoComplete ]
               }

        allSymptomSuggestions = symptomSuggestions self.props.items

        symptomTypeText {val}=
          text { key: "symptom label"
               , children: [ string $ val]
               }

        itemSelected entry =
          send self $ TextSelected entry

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
              , maybe autoCompWrapped symptomTypeText self.state.selectedText
              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: (capture_ $ send self SaveItem )
                       }
              ]}

inputChangeHandler :: List AC.Entry -> Maybe String -> List AC.Entry -> Int -> Effect AC.Response
inputChangeHandler symptoms' mtext _entries nextId =
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
      MU.symptomItemEntryDescriptions #
      Util.filterEmptyStrings #
      Array.nubEq

    allSuggestions = suggestions <> providedSymptoms
  in
   AC.arrayToEntries allSuggestions
