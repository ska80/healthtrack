module HealthTrack.ItemEntryScreen.Food where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import HealthTrack.Model (ItemEntry(..), Item)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (text, string, button, view, textInput, flatList)
import Effect.Now (now)
import HealthTrack.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (fromFoldable)
import HealthTrack.Util (toListRenderItem)
import Data.String as Str
import Data.List as List
import Data.List (List)
import Data.Maybe (maybe)
import Data.Array (mapWithIndex)
import Data.Array as Array
import HealthTrack.AutoComplete as AC

import Debug.Trace (spy)

comp :: Component Props
comp = createComponent "AddFoodEntryScreen"

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
            -- spy' = spy "TextSelected" entry
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
                    FoodItem $ maybe "" _.val self'.state.selectedText
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
                                          , initialEntries: foods'
                                          , addCreateEntry: true
                                          , handler: inputChangeHandler
                                          }
                        ]
            }

        foods' = foodSuggestions self.props.items

        foodTypeText {val}=
          text { key: "food label"
               , children: [ string $ val]
               }

        itemSelected entry =
          send self $ TextSelected entry

        setStateText tv =
          self.setState _ { textVal = tv }

        inputChangeHandler mtext entries nextId =
          pure $ AC.Response (maybe foods' filterEntries mtext) nextId
          where
            filterEntries :: String -> List AC.Entry
            filterEntries str =
              List.filter (hasStr str) foods'

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
                     , children: [ string "Food type:" ]
                     }
              , maybe autoComp foodTypeText self.state.selectedText
              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: (capture_ $ send self SaveItem )
                       }
              ]}

providedFoods :: Array String
providedFoods =
  [ "headache"
  , "stomach pain"
  , "joint pain - back"
  , "joint pain - hands"
  , "joint pain - neck"
  ]

foodSuggestions :: Array Item -> List AC.Entry
foodSuggestions items =
  let
    entries = _.entry <$> items

    foodNameFromItemEntry :: ItemEntry -> Maybe String
    foodNameFromItemEntry entry =
      case entry of
        FoodItem s ->
          if Str.length s > 0 then
            Just s
          else
            Nothing
        _ -> Nothing

    names = Array.mapMaybe foodNameFromItemEntry entries

    uniqueNames = Array.nubEq names

    allSuggestions = uniqueNames <> providedFoods
  in
   AC.arrayToEntries allSuggestions