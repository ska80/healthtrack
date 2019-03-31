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
-- type Props a = forall a .
  { onEntryComplete :: ItemEntry -> Effect Unit
  , key :: String
  , initialEntries :: Array String
  , addCreateEntry :: Boolean
  , handler :: Action -> Effect Unit
  -- , userState :: a
    -- or maybe have some kind of a callback thing that controls everything?
    -- onTextChange -- do something whenever input chagnes
    -- onCreateNew -- user presses return or clicks the `Create "whatever"` btn
    -- onItemSelected -- when user clicks something, or preses return for create new
    -- etc
  }

type State =
-- type State a = forall a .
  { textVal :: Maybe String
  , entries :: List Entry
  , nextId :: Int
  -- , userState :: a
  }

type Entry =
  { key :: String
  , val :: String
  }

-- comp :: forall a . Component (Props a)
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
  | AddItem

-- autoComplete :: forall a . Props a -> JSX
autoComplete :: Props -> JSX
autoComplete props = make comp
  { render
  , initialState
  } props
  where
    initialState =
      { textVal: Nothing
      , entries: initialEntries
      -- , userState: props.userState
      , nextId
      }
    initialEntries = fixupInitialEntries props.initialEntries
    nextId = List.length initialEntries

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
          NoUpdate

        AddItem ->
          NoUpdate

    send = runUpdate update

    -- renderItem :: forall a . Self (Props a) (State a) -> {item :: Entry} -> JSX
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
          {
            key: "CREATE_OPTION"
          ,
            val: "Create \"" <> val <> "\""
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

    -- make take 5 portion configurable somehow?
    -- numOptionsToShow = 5

    render :: Self Props State -> JSX
    render self =
      let
        entries :: List Entry
        entries = entriesWithCreateOption self
      in
       -- keyboardAvoidingView
       view { style: css {
                 -- ,
                 -- padding: 10
                 -- ,
                 width: "100%"
                 -- ,
                 -- flexDirection: "column"
                 -- ,
                 -- flex: 1
                 -- ,
                 -- justifyContent: "center"
                 -- ,
                 -- alignItems: "stretch"
                 -- ,
                 -- borderColor: "green"
                 -- ,
                 -- borderWidth: 1
                 -- ,
                 -- height: 500
                 }

              -- , behavior: toKbdAvdPropBehv "padding"
              -- , enabled: true
            ,
              key: self.props.key
            ,
              children:
              [
                textInput {
                   key: "symptomTypeInput"
                   ,
                   placeholder: "Enter here"
                   ,
                   style: css {
                     borderWidth: 1
                     ,
                     borderColor: "black"
                     ,
                     width: 200
                     ,
                     height: 30
                     }
                   ,
                   onChange: (capture Util.getText (send self <<< InputUpdate))
                   ,
                   value: maybe "" identity self.state.textVal
                   ,
                   onSubmitEditing: (capture_ $ send self AddItem)
                   ,
                   autoCorrect: false
                   }
              ,
                view {
                   style: css {height: 200}
                   ,
                   key: "flatListWrap"
                   ,
                   children: [
                     flatList {
                        data: unsafeCoerce $ fromFoldable $ entries
                        ,
                        key: "itemsList"
                        ,
                        renderItem: toListRenderItem $ renderItem self
                        ,
                        style: css {
                          -- height: 200
                          -- ,
                          -- borderColor: "blue"
                          -- ,
                          -- borderWidth: 1
                                   }
                        }
                     ]
                   }
              ]
            }

nextAutocompEntries :: Maybe String -> State -> List Entry
nextAutocompEntries input state =
  let
    idn = show state.nextId
    input' = maybe "" identity input
  in
   {key: idn, val: input' <> idn } : state.entries



toKbdAvdPropBehv :: String -> KeyboardAvoidingViewPropsBehavior
toKbdAvdPropBehv = unsafeCoerce
