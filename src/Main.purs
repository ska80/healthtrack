module Main where

import Prelude

import Effect (Effect)
import React (ReactClass, statelessComponent, component, createLeafElement, ReactElement, getState)
import ReactNative.API (registerComponent)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view, view_)
import ReactNative.Components.Button (button_)
import ReactNative.Components.ListView (listView, listViewDataSource, ListViewDataSource)
import ReactNative.Styles (Styles, staticStyles, marginTop)
-- import Dispatcher.React (getProps, getState, modifyState, renderer, saveRef, withRef)
import Dispatcher.React (renderer -- , getState
                        )
-- import Debug.Trace

itemsListClass :: ReactClass {}
itemsListClass = component "ItemsList" spec
  where
    spec this = do
      pure { render: render <$> getState this -- renderer render this
           , state: initialState}
      where
        -- foo = spy "this = " this
        render state =
          let
            ds = (listViewDataSource [])
          -- state <- getState this'
          in
          view_
            [ button_ "A Button"
            , listView state.dataSource rowRender
            , text_ "Hello again"
            ]
        rowRender x = text_ "item"
      -- pure {render: renderer render this, state: initialState}

type ItemsListProps = {}

type State =
  { nextId :: Int
  , items :: Array String
  , dataSource :: ListViewDataSource String
  }

initialState :: State
initialState =
  { nextId: 0
  , items: []
  , dataSource: listViewDataSource []
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
