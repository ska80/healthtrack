module Main where

import Model (AppState, Item)
import Prelude (($), (+), (<>), show, identity, Unit, unit, discard, bind, const)
import Effect.Console (log)
import Effect (Effect(..))
import Control.Applicative (pure)
-- import Data.List (List(..), (:))
import Data.Array (fromFoldable, (:))
import Data.Maybe (Maybe(..), maybe)
-- import Data.Tuple (Tuple(..))
import Data.Nullable (toMaybe)
import React.Basic (Self, StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (text_, text, string, button, view, textInput, flatList)
import Storage as Storage
import Unsafe.Coerce (unsafeCoerce)

import Effect.Aff
import Effect.Class
import Effect.Aff.Compat
import Data.Either




comp :: Component {}
comp = createComponent "Main"

data Action
  = AddItem


initialState :: AppState
initialState = { nextId: 0,
                 textVal: Nothing,
                 items: [] }

main :: JSX
main = make comp
  { render,
    initialState,
    didMount
  -- , didUpdate
    } {}
  where
    didMount :: Self {} AppState -> Effect Unit
    didMount self = do
      log "Main has initialized"
      _ <- launchAff do
        liftEffect (log "loading AppState")
        loaded <- Storage.retrieve
        let appState =
              case loaded of
                Left errors -> do
                  liftEffect $ log "Errors loading:"
                  liftEffect $ log $ show errors
                  liftEffect $ log "end Errors loading:"
                  pure initialState
                Right (state :: AppState) -> do
                  liftEffect $ log "loaded AppState"
                  pure state
        self.setState(\b -> appState)
        log "Launched!"
      pure unit

    didUpdate self = do
      launchAff $ Storage.store self.state


    update self =
      case _ of
        AddItem ->
          let
            nextEntry = { key: show self.state.nextId
                        , val: maybe "" identity self.state.textVal }
          in
           Update $ self.state {
             items =  nextEntry : self.state.items,
             nextId = self.state.nextId + 1,
             textVal = Nothing
             }

    send = runUpdate update

    render self =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ debugLabelInfoView self
             , textInput { key: "txtinput"
                         , placeholder: "type here"
                         , style: css { flex: 1 }
                         , onChange: (capture getText setStateText)
                         , value: maybe "" identity self.state.textVal
                         , onSubmitEditing: (capture_ $ send self AddItem )
                         , autoCorrect: false
                         }
             , flatList { data: unsafeCoerce $ fromFoldable self.state.items
                        , key: "itemsList"
                        , renderItem: unsafeCoerce (\{item}-> text { children: [ string item.val ] })
                        }
             , button { title: "save"
                      , key: "clickyButton"
                      , onPress: (capture_ $ send self AddItem )
                      }
             ]
           }
        where
          setStateText tv =
            self.setState _ { textVal = tv }

showDebugInfo :: Boolean
-- showDebugInfo = false
showDebugInfo = true

debugLabelInfoView :: Self {} AppState -> JSX
debugLabelInfoView self =
  if showDebugInfo then
    let
      debugMsg = "debug: " <> show self.state.nextId <>
                 ";" <> show self.state.textVal <>
                 ";" <> show self.state.items
    in
     text { key: "debugLabelInfo",
            children: [string debugMsg]
          }
  else
    view {key: "debugLabelInfoEmpty", children: []}

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
