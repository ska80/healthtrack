module LogEntryScreen where

import Model (AppState, Screens(..))

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


import Effect.Aff
import Effect.Class (liftEffect)

import Data.Either (Either(..))

comp :: Component Props
comp = createComponent "Main"

data Action
  = AddItem

initialState :: AppState
initialState = { nextId: 0
               , textVal: Nothing
               , items: []
               , currentScreen: MenuScreen
               }

type Props =
  { state :: AppState
  , onStateUpdate :: AppState -> Effect Unit
  }

logEntryScreen :: Props -> JSX
logEntryScreen props = make comp
  { render
  , initialState: props.state
  } props
  where
    updateParent self = do
      log "logEntryScreen updateParent"
      self.props.onStateUpdate self.state

    update self =
      case _ of
        AddItem ->
          let
            nextEntry =
              { key: show self.state.nextId
              , val: maybe "" identity self.state.textVal
              }

            nextState = self.state
              { items =  nextEntry : self.state.items
              , nextId = self.state.nextId + 1
              , textVal = Nothing
              }
          in
           UpdateAndSideEffects nextState updateParent

    send = runUpdate update

    render self =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ textInput { key: "txtinput"
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

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
