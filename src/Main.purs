module Main where

-- import Prelude
import Prelude (Unit, const, not, pure, show, ($), (+), (<<<), (<>))

import Effect (Effect)
import React (ReactClass, statelessComponent, component, createLeafElement, ReactElement -- , getState
             )
import ReactNative.API (registerComponent)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view, view_)
import ReactNative.Components.Button (button)
import ReactNative.Components.ListView (listView, listViewDataSource)
import ReactNative.Styles (Styles, staticStyles, marginTop)
-- import Dispatcher.React (getProps, getState, modifyState, renderer, saveRef, withRef)

import Dispatcher (-- action,
                   affAction, mkDispatch1, Dispatch1(..))
import Dispatcher.React (modifyState, renderer)

-- import Data.Semigroup ((<>))
-- import Data.Show (show)
-- import Data.Array
import Data.Array ((:))


data Action = ToggleState

itemsListClass :: ReactClass {}
itemsListClass = component "ItemsList" spec
  where

    eval ToggleState =
      modifyState (\state -> state { on = not state.on
                                   , items = ("item: " <> show state.nextId) : state.items
                                   , nextId = state.nextId + 1
                                   })

    spec this = do
      pure { render: renderer render this
           , state: initialState }
      where
        de = affAction this <<< eval
        (Dispatch1 d) = mkDispatch1 de
        render {state,props} =
          view_
             [ button "A Button" $ d (const ToggleState)
             , text_ $ "current state: " <> (show state.on)
             , listView (listViewDataSource state.items) text_
             ]
        rowRender x = text_ "item"

type ItemsListProps = {}

type State =
  { nextId :: Int
  , items :: Array String
  , on :: Boolean
  }

initialState :: State
initialState =
  { nextId: 0
  , items: []
  , on: false
  }

itemsList :: ItemsListProps -> ReactElement
itemsList = createLeafElement itemsListClass

app :: ReactClass {}
app = statelessComponent render
  where
    render _ = view sheet.container
               [ text_ "This is a list!"
               , itemsList {}
               ]

sheet :: {
    container :: Styles
}
sheet = {
  container: staticStyles [
      marginTop 64
  ]
}


main :: Effect Unit
main = do
  registerComponent "HealthTrack" app
