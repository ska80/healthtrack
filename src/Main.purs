module Main where

import Model (AppState, Screens(..), initialState)

import Prelude (($), (+), (<>), show, identity, Unit, unit, discard, bind, const)
import Effect.Console (log)
import Effect (Effect)
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
import React.Basic.Native (text, string, button, view, textInput, flatList)
import Storage as Storage
import Unsafe.Coerce (unsafeCoerce)

import MenuScreen as MenuScreen
import LogEntryScreen as LogEntryScreen


import Effect.Aff
import Effect.Class (liftEffect)

import Data.Either (Either(..))

comp :: Component {}
comp = createComponent "Main"

data Action
  = ChangeScreen Screens

main :: JSX
main = make comp
  { render
  , initialState
  , didMount
  , didUpdate
  } {}
  where
    didMount :: Self {} AppState -> Effect Unit
    didMount self = do
      log "Main has initialized (didMount)"
      _ <- launchAff do
        liftEffect (log "loading AppState (didMount Aff)")
        loaded <- Storage.retrieve
        let
          appState :: Aff AppState
          appState =
              case loaded of
                Left errors -> do
                  liftEffect (log "Errors loading:" :: Effect Unit)
                  liftEffect $ log $ show errors
                  liftEffect $ log "end Errors loading:"
                  pure initialState
                Right (state :: AppState) -> do
                  liftEffect $ log "loaded AppState"
                  liftEffect $ log $ show state
                  pure state
        appState' <- appState

        -- if we left off on another screen, ensure we return to this screen
        let appState'' = appState' { currentScreen = MenuScreen }

        liftEffect $ self.setState(const appState'')
        liftEffect $ (log "Launched! (didMount Aff Finale)" :: Effect Unit)
      pure unit

    didUpdate self prev = do
      _ <- launchAff do
        liftEffect $ log "Storing state (didUpdate)"
        Storage.store self.state
        liftEffect $ log "done Storing state (didUpdate)"
      pure unit

    update self =
      case _ of
        ChangeScreen screen ->
          UpdateAndSideEffects
            (self.state { currentScreen = screen })
            (\self' -> log ("ChangingScreen " <> show screen))

    send = runUpdate update
    render self =
      case self.state.currentScreen of
        MenuScreen ->
          MenuScreen.menu { onMenuClick: \screen ->
                            send self (ChangeScreen AddItemScreen)}
        AddItemScreen ->
          LogEntryScreen.logEntryScreen { state: self.state
                                        , onStateUpdate:
                                          \newState ->
                                             self.setState (\s-> newState)
                                        }

        DeveloperScreen -> view {}
