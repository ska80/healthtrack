module Main where

import Prelude

import Effect (Effect)
import React (ReactClass, statelessComponent, component, createLeafElement, ReactElement -- , getState
             )
import ReactNative.API (registerComponent)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view, view_)
import ReactNative.Components.Button (button)
import ReactNative.Components.ListView (listView, listViewDataSource, ListViewDataSource)
import ReactNative.Styles (Styles, staticStyles, marginTop)
-- import Dispatcher.React (getProps, getState, modifyState, renderer, saveRef, withRef)

import Dispatcher (action, affAction, mkDispatch1, Dispatch1(..))
import Dispatcher.React (modifyState, renderer)

data Action = ToggleState

itemsListClass :: ReactClass {}
itemsListClass = component "ItemsList" spec
  where

    eval ToggleState =
      modifyState (\state -> state { on = not state.on })

    spec this = do
      pure { render: renderer render this
           , state: initialState }
      where
        de = affAction this <<< eval
        (Dispatch1 d) = mkDispatch1 de
        render {state,props} =
          let
            ds = (listViewDataSource [])
          in
           view_
             [ button "A Button" $ d \x -> ToggleState
             , listView state.dataSource rowRender
             , text_ (show state.on)
             ]
        rowRender x = text_ "item"

type ItemsListProps = {}

type State =
  { nextId :: Int
  , items :: Array String
  , dataSource :: ListViewDataSource String
  , on :: Boolean
  }

initialState :: State
initialState =
  { nextId: 0
  , items: []
  , dataSource: listViewDataSource []
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
