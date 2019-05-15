module HealthTrack.AutoComplete where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (button, flatList, textInput, view)
import HealthTrack.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Data.List (List, (:))
import Data.List as List
import HealthTrack.Util (toListRenderItem)
import Data.Array as Array

type Props =
  { onItemSelected :: Entry -> Effect Unit
  , key :: String
  , initialEntries :: List Entry
  , addCreateEntry :: Boolean
  , handler :: Maybe String -> List Entry -> Int -> Effect Response
  }

type State =
  { textVal :: Maybe String
  , entries :: List Entry
  , nextId :: Int
  }

type Entry =
  { key :: String
  , val :: String
  , displayText :: String
  }

comp :: Component Props
comp = createComponent "AutoComplete"


data Action
  = InputUpdate (Maybe String)
  | EntryPress Entry
  | AddItem

data Response = Response (List Entry) Int

autoComplete :: Props -> JSX
autoComplete props = make comp
  { render
  , initialState
  } props
  where
    initialState =
      { textVal: Nothing
      , entries: initialEntries
      , nextId: List.length initialEntries
      }

    initialEntries = props.initialEntries

    update :: Self Props State -> Action -> StateUpdate Props State
    update self action =
      case action of
         InputUpdate mtext ->
           let
             idn = show self.state.nextId
             entriesE = self.props.handler mtext self.state.entries self.state.nextId
             state' = self.state { textVal = mtext }
           in
            UpdateAndSideEffects state' \self' -> do
              (Response entries nextId) <- entriesE
              self'.setState $ \s-> s { entries = entries
                                      , nextId = nextId
                                      }

         EntryPress entry ->
           SideEffects \self' -> self'.props.onItemSelected entry

         AddItem ->
           let
             itemText = maybe "" identity self.state.textVal

             nextId' = self.state.nextId + 1

             newEntry = { key: show self.state.nextId
                        , val: itemText
                        , displayText: itemText
                        }

             newState = self.state { nextId = nextId' }

             sendComplete self' = self'.props.onItemSelected newEntry
           in
            UpdateAndSideEffects newState sendComplete


    send = runUpdate update

    renderItem :: Self Props State -> {item :: Entry} -> JSX
    renderItem self {item} =
      let
        backgroundColor =
          if item.key == "CREATE_OPTION" then
            "lightgrey"
          else
            "transparent"
      in
       view { style: css { backgroundColor }
            , children: [
                 button { title: item.val
                        , onPress: capture_ (send self $ EntryPress item )
                        }
                 ]
            }
    entriesWithCreateOption :: Self Props State -> List Entry
    entriesWithCreateOption self =
      let
        state = self.state
        mval = state.textVal
        entries = state.entries

        newEntryForText val =
          { key: "CREATE_OPTION"
          , displayText: "Create \"" <> val <> "\""
          , val: val
          }
      in
       if self.props.addCreateEntry then
         case mval of
           Just text ->
             newEntryForText text : entries
           Nothing ->
             entries
       else
         entries

    render :: Self Props State -> JSX
    render self =
      let
        entries :: List Entry
        entries = entriesWithCreateOption self
      in
       view { style: css { width: "100%" }
            , key: self.props.key
            , children:
              [
                textInput { key: "autocompletetextinput"
                          , placeholder: "Enter here"
                          , style: css { borderWidth: 1
                                       , borderColor: "black"
                                       , width: 200
                                       , height: 30
                                       }
                          , onChange: (capture Util.getText (send self <<< InputUpdate))
                          , value: maybe "" identity self.state.textVal
                          , onSubmitEditing: (capture_ $ send self AddItem)
                          , autoCorrect: false
                          }
              , view { style: css {height: 200}
                     , key: "flatListWrap"
                     , children: [
                       flatList {
                          data: unsafeCoerce $ Array.fromFoldable $ entries
                          , key: "itemsList"
                          , renderItem: toListRenderItem $ renderItem self
                          , style: css { }
                          }
                       ]
                     }
              ]
            }

-- convert list of strings to a list of entries
-- useful for populating the autosuggest list
arrayToEntries :: Array String -> List Entry
arrayToEntries =
  List.fromFoldable <<< Array.mapWithIndex toEntry
  where
    toEntry id val = { key: show id, val: val, displayText: val }
