module HealthTrack.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import HealthTrack.ItemEntryScreen.AddEntryScreen as AddEntryScreen
import HealthTrack.DeveloperScreen as DeveloperScreen
import HealthTrack.MenuScreen as MenuScreen
import HealthTrack.Model (AppState, Screen(..), initialState)
import HealthTrack.Storage as Storage
import HealthTrack.ViewLogScreen as ViewLogScreen
import React.Basic (Self, StateUpdate(..), JSX, make, runUpdate, Component, createComponent)

comp :: Component {}
comp = createComponent "Main"

data Action
  = ChangeScreen Screen

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
        loaded <- Storage.loadAndInitializeAppState
        let
          appState :: Aff AppState
          appState =
              case loaded of
                Left errors -> do
                  liftEffect (log "Errors loading:" :: Effect Unit)
                  liftEffect $ log $ show errors
                  liftEffect $ log "End Errors loading"
                  pure initialState
                Right (state :: AppState) -> do
                  liftEffect $ log "loaded AppState"
                  liftEffect $ log $ show state
                  pure state
        appState' <- appState

        -- -- if we left off on another screen, ensure we return to this screen
        -- let appState'' = appState' { currentScreen = MenuScreen }

        liftEffect $ self.setState(const appState')
        liftEffect $ (log "Launched! (didMount Aff Finale)" :: Effect Unit)
      pure unit

    didUpdate self prev = do
      _ <- launchAff do
        liftEffect $ log "Storing state (didUpdate)"
        Storage.storeAppState self.state
        liftEffect $ log "done Storing state (didUpdate)"
      pure unit

    update self =
      case _ of
        ChangeScreen screen ->
          UpdateAndSideEffects
            (self.state { currentScreen = screen })
            (\self' -> log ("ChangingScreen " <> show screen))

    send = runUpdate update

    changeScreen self screen =
      send self (ChangeScreen screen)

    render self =
      case self.state.currentScreen of
        MenuScreen ->
          MenuScreen.menu { onMenuClick: changeScreen self }
        AddItemScreen ->
          AddEntryScreen.logEntryScreen { state: self.state
                                        , onStateUpdate:
                                          \newState ->
                                             self.setState (\s-> newState)
                                        , returnToMenuE: changeScreen self MenuScreen
                                        , changeScreen: changeScreen self
                                        }
        ViewLogScreen ->
          ViewLogScreen.viewLogScreen
            { returnToMenuE: changeScreen self MenuScreen
            , state: self.state
            , changeScreen: changeScreen self
            }

        DeveloperScreen ->
          DeveloperScreen.developerScreen
            { returnToMenuE: changeScreen self MenuScreen
            , appState: self.state
            }
