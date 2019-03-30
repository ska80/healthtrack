module HealthTrack.AutoComplete where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import HealthTrack.Model (ItemEntry(..))
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent, Self)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (text, string, button, view, keyboardAvoidingView, textInput, flatList, KeyboardAvoidingViewPropsBehavior)
import Effect.Now (now)
import HealthTrack.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Data.Array (fromFoldable, mapWithIndex)
import Data.List (List(..), (:), take)
import Data.List as List
import HealthTrack.Util (toListRenderItem)
import Debug.Trace (spy)

type Props =
  { onEntryComplete :: ItemEntry -> Effect Unit
  , key :: String
  , initialEntries :: Array String
    -- TODO pass in data??
    -- or maybe have some kind of a callback thing that controls everything?
    -- onTextChange -- do something whenever input chagnes
    -- onCreateNew -- user presses return or clicks the `Create "whatever"` btn
    -- onItemSelected -- when user clicks something, or preses return for create new
    -- etc
  }

type State =
  { textVal :: Maybe String
  , entries :: List Entry
  , nextId :: Int
  }

type Entry =
  { key :: String
  , val :: String
  }

-- TODO create component
-- render list
-- rerun list logic on text input
--

comp :: Component Props
comp = createComponent "AutoComplete"

fixupInitialEntries :: Array String -> List Entry
fixupInitialEntries =
  List.fromFoldable <<< mapWithIndex toEntry
  where
    toEntry id val = { key: show id, val }

data Action
  = InputUpdate (Maybe String)
  | EntryPress Entry

autoComplete :: Props -> JSX
autoComplete props = make comp
  { render
  , initialState: { textVal: Nothing, entries: initialEntries, nextId}
  } props
  where
    initialEntries = fixupInitialEntries props.initialEntries
    nextId = List.length initialEntries
    -- TODO make this user-configrable
    nextAutocompEntries :: Maybe String -> State -> List Entry
    nextAutocompEntries input state =
      let
        idn = show state.nextId
        input' = maybe "" identity input
      in
       {key: idn, val: input' <> idn } : state.entries

    update :: Self Props State -> Action -> StateUpdate Props State
    update self action =
      case action of
        InputUpdate mtext ->
          let
            idn = show self.state.nextId
            entries = nextAutocompEntries mtext self.state
          in
           Update $ self.state { textVal = mtext
                               , entries = entries
                               , nextId = self.state.nextId + 1
                               }
        EntryPress entry ->
          let foo = spy "entryPress" entry in
          NoUpdate

    send = runUpdate update

    renderItem :: Self Props State -> {item :: Entry} -> JSX
    renderItem self {item} =
      let
        backgroundColor =
          if item.key == "CREATE_OPTION" then
            "steelblue"
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

    entriesWithCreateOption :: State -> List Entry
    entriesWithCreateOption state =
      let
        mval = state.textVal
        entries = state.entries

        newEntryForText val =
          { key: "CREATE_OPTION"
          , val: "Create \"" <> val <> "\""
          }
       in
        case mval of
          Just text ->
            newEntryForText text : entries
          Nothing ->
            state.entries

    render :: Self Props State -> JSX
    render self =
      let
        entries :: List Entry
        entries = entriesWithCreateOption self.state

      in
       keyboardAvoidingView { -- style: css {
                              --    -- flexDirection: "column",
                              --    -- padding: 50
                              --    -- ,
                              --    width: "100%"
                              --    -- , height: "100%"
                              --    }
                            -- ,
                            behavior: toKbdAvdPropBehv "padding"
                            , enabled: true
                            , key: self.props.key
                            , children:
                              -- make take 5 portion configurable somehow?
                              [ flatList { data: unsafeCoerce $ fromFoldable $ take 5 entries
                                         , key: "itemsList"
                                         , renderItem: toListRenderItem $ renderItem self
                                         }

                              , textInput { key: "symptomTypeInput"
                                          , placeholder: "Enter type here, or select from list"
                                          , style: css {

                                            -- flex: 1
                                            --            ,
                                            borderWidth: 1
                                            , borderColor: "black"
                                            , padding: 5
                                            , width: "100%"
                                            }
                                          , onChange: (capture Util.getText (send self <<< InputUpdate))
                                          , value: maybe "" identity self.state.textVal
                                            -- , onSubmitEditing: (capture_ $ send self AddItem )
                                            -- TODO maybe reenable autocorrect? seems like there
                                            -- should be a better way to fix the weird way the
                                            -- app was re-populating the field. idk.
                                          , autoCorrect: false
                                            -- , multiline: true
                                          }
                              ]
                              }
toKbdAvdPropBehv :: String -> KeyboardAvoidingViewPropsBehavior
toKbdAvdPropBehv = unsafeCoerce
