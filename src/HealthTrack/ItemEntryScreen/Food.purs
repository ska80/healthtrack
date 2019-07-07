module HealthTrack.ItemEntryScreen.Food where

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
-- import Debug.Trace (spy)

comp :: Component Props
comp = createComponent "AddFoodEntryScreen"

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
  { selectedText :: Maybe AC.Entry
  , note :: Maybe String
  }

form :: Props -> JSX
form props = make comp
  { render
  , initialState: { selectedText: Nothing
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
                    FoodItem { name: nextName, note: nextNote }
                self'.props.onEntryComplete nextEntry

    send = runUpdate update

    render self =
      let
        renderItem {item} = text { children : [ string "string" ] }

        nameAutoComplete =
          AC.autoComplete { onItemSelected: itemSelected
                          , key: "nameAutoComplete"
                          , initialEntries: allFoodSuggestions
                          , addCreateEntry: true
                          , handler: inputChangeHandler allFoodSuggestions
                          }

        autoCompWrapped =
          view { key: "NameWrapper"
               , style: css { height: "100%"
                            , width: "100%"
                            }
               , children: [ nameAutoComplete ]
               }

        allFoodSuggestions = foodSuggestions self.props.items

        foodTypeText {val} =
          text { key: "food label"
               , children: [ string $ val]
               }

        itemSelected entry =
          send self $ TextSelected entry

        setNote tv =
          self.setState _ { note = tv }

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
              , maybe autoCompWrapped foodTypeText self.state.selectedText

              , CV.notesInput self.state.note setNote (send self SaveItem)

              , button { title: "save"
                       , key: "clickyButton"
                       , onPress: (capture_ $ send self SaveItem )
                       }
              ]
            }

inputChangeHandler :: List AC.Entry -> Maybe String -> List AC.Entry -> Int -> Effect AC.Response
inputChangeHandler foods' mtext _entries nextId =
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

providedFoods :: Array String
providedFoods =
  [ "candy"
  , "sandwich"
  , "salad"
  , "meat"
  ]

foodSuggestions :: Array Item -> List AC.Entry
foodSuggestions items =
  let
    getItemNameStr :: ItemName -> String
    getItemNameStr =
      case _ of
        ItemName name -> name

    -- pull names from item entries for suggestions
    suggestions =
       items <#>
      _.entry #
      MU.foodItemEntryDescriptions <#>
      getItemNameStr #
      Util.filterEmptyStrings #
      Array.nubEq

    allSuggestions = suggestions <> providedFoods
  in
   AC.arrayToEntries allSuggestions
