module Main where

import Prelude

import Effect (Effect)
import React (ReactClass, statelessComponent, component, createLeafElement, ReactElement)
import ReactNative.API (registerComponent)
import ReactNative.Components.Text (text_)
import ReactNative.Components.View (view)
import ReactNative.Styles (Styles, staticStyles, marginTop)
-- import Dispatcher.React (getProps, getState, modifyState, renderer, saveRef, withRef)
import Dispatcher.React (renderer)

itemsListClass :: ReactClass {}
itemsListClass = component "ItemsList" spec
  where
    spec this = do
      let render _ = text_ "Hello again"
      pure {render: renderer render this , state: initialState}

type ItemsListProps = {}

type State =
  { nextId :: Int
  , items :: Array String
  }

initialState :: State
initialState =
  { nextId: 0
  , items: []
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
