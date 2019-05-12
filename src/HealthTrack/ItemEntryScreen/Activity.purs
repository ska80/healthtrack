module HealthTrack.ItemEntryScreen.Activity where

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
comp = createComponent "AddActivityEntryScreen"

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
  }


initialState

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
            nextState = self.state { selectedText = Just entry }
          in
           Update nextState

        SaveItem ->
          SideEffects doSaveItem
            where
              doSaveItem :: Self Props State -> Effect Unit
              doSaveItem self' = do
                let
                  nextEntry =
                    ActivityItem $ maybe "" _.val self'.state.selectedText
                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      let
        renderItem {item} = text { children : [ string "string" ] }

        descriptionAutoComplete =
          AC.autoComplete { onItemSelected: itemSelected
                          , key: "descriptionAutoComplete"
                          , initialEntries: allActivitySuggestions
                          , addCreateEntry: true
                          , handler: inputChangeHandler allActivitySuggestions
                          }

        autoCompWrapped =
          view { key: "DescriptionWrapper"
               , style: css { height: "100%"
                            , width: "100%"
                            }
               , children: [ descriptionAutoComplete ]
               }

        allActivitySuggestions = activitySuggestions self.props.items

        activityTypeText {val}=
          text { key: "activity label"
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
                     , children: [ string "Activity type:" ]
                     }
              , maybe autoCompWrapped activityTypeText self.state.selectedText
              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: (capture_ $ send self SaveItem )
                       }
              ]}

inputChangeHandler :: List AC.Entry -> Maybe String -> List AC.Entry -> Int -> Effect AC.Response
inputChangeHandler activitys' mtext _entries nextId =
  pure $ AC.Response (maybe activitys' filterEntries mtext) nextId
  where
    filterEntries :: String -> List AC.Entry
    filterEntries str =
      List.filter (hasStr str) activitys'

    hasStr :: String -> AC.Entry -> Boolean
    hasStr str =
      let
        entryPattern = Str.Pattern (Str.toLower str)
      in
       Str.contains entryPattern <<< Str.toLower <<< _.val

providedActivitys :: Array String
providedActivitys =
  [ "walk"
  , "jog"
  , "resistance training"
  , "yoga"
  , "meditation"
  ]

activitySuggestions :: Array Item -> List AC.Entry
activitySuggestions items =
  let
    -- pull names from item entries for suggestions
    suggestions =
      items <#>
      _.entry #
      MU.activityItemEntryDescriptions #
      Util.filterEmptyStrings #
      Array.nubEq

    allSuggestions = suggestions <> providedActivitys
  in
   AC.arrayToEntries allSuggestions
