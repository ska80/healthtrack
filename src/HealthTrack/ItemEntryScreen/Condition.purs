module HealthTrack.ItemEntryScreen.Condition where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import HealthTrack.Model (ItemEntry(..), Item, ItemName(..), ItemNote(..))
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
import HealthTrack.CommonViews as CV

comp :: Component Props
comp = createComponent "AddConditionEntryScreen"

data Action
  = SaveItem
  | TextSelected AC.Entry

type Props =
  { onEntryComplete :: ItemEntry -> Effect Unit
  , key :: String
  , items :: Array Item
  , item :: Maybe Item
  }

type State =
  { description :: Maybe String
  , selectedText :: Maybe AC.Entry
  , note :: Maybe String
  }

form :: Props -> JSX
form props = make comp
  { render
  , initialState: { description: Nothing
                  , selectedText: Nothing
                  , note: Nothing
                  }
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
                  nextName = ItemName $ maybe "" _.val self'.state.selectedText
                  nextNote = ItemNote $ maybe "" identity self'.state.note
                  nextEntry =
                    ConditionItem { name: nextName, note: nextNote }

                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      let
        renderItem {item} = text { children : [ string "string" ] }

        descriptionAutoComplete =
          AC.autoComplete { onItemSelected: itemSelected
                          , key: "descriptionAutoComplete"
                          , initialEntries: allConditionSuggestions
                          , addCreateEntry: true
                          , handler: inputChangeHandler allConditionSuggestions
                          }

        autoCompWrapped =
          view { key: "DescriptionWrapper"
               , style: css { height: "100%"
                            , width: "100%"
                            }
               , children: [ descriptionAutoComplete ]
               }

        allConditionSuggestions = conditionSuggestions self.props.items

        conditionTypeText {val}=
          text { key: "condition label"
               , children: [ string $ val]
               }

        itemSelected entry =
          send self $ TextSelected entry

        setNote tv =
          self.setState _ { note = tv }

        doSave = send self SaveItem

      in
       view { style: css { flexDirection: "column"
                         , padding: 50
                         , width: "100%"
                         , height: "100%"
                         }
            , key: self.props.key
            , children:
              [ text { key: "instructions"
                     , children: [ string "Condition type:" ]
                     }
              , maybe autoCompWrapped conditionTypeText self.state.selectedText

              , CV.notesInput self.state.note setNote doSave

              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: capture_ doSave
                       }
              ]}

inputChangeHandler :: List AC.Entry -> Maybe String -> List AC.Entry -> Int -> Effect AC.Response
inputChangeHandler conditions' mtext _entries nextId =
  pure $ AC.Response (maybe conditions' filterEntries mtext) nextId
  where
    filterEntries :: String -> List AC.Entry
    filterEntries str =
      List.filter (hasStr str) conditions'

    hasStr :: String -> AC.Entry -> Boolean
    hasStr str =
      let
        entryPattern = Str.Pattern (Str.toLower str)
      in
       Str.contains entryPattern <<< Str.toLower <<< _.val

providedConditions :: Array String
providedConditions =
  [ "highly stressed"
  , "bad sleep"
  ]

conditionSuggestions :: Array Item -> List AC.Entry
conditionSuggestions items =
  let
    -- pull names from item entries for suggestions
    suggestions =
      items <#>
      _.entry #
      MU.conditionItemEntryDescriptions #
      Util.filterEmptyStrings #
      Array.nubEq

    allSuggestions = suggestions <> providedConditions
  in
   AC.arrayToEntries allSuggestions
