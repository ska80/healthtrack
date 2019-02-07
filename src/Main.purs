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
import ReactNative.Components.TextInput ( textInput')
import ReactNative.Styles (Styles, staticStyles, marginTop)
import ReactNative.PropTypes (center, unsafeRef)
import ReactNative.Styles (Styles, flex, height, marginTop, padding, paddingLeft, staticStyles, width)

import Dispatcher (-- action,
                   affAction, mkDispatch1, Dispatch1(..))
import Dispatcher.React (modifyState, renderer)

-- import Data.Semigroup ((<>))
-- import Data.Show (show)
-- import Data.Array
import Data.Array ((:))
import ReactNative.Styles (Styles, flex, height, marginTop, padding, paddingLeft, staticStyles, width)
import ReactNative.Styles.Flex (alignItems, flexDirection, row)
import ReactNative.Styles.Text (fontSize)

data Action = AddItem | InputChange String

itemsListClass :: ReactClass {}
itemsListClass = component "ItemsList" spec
  where
    eval input =
      case input of
        AddItem ->
          modifyState (\state ->
                        state { items = ("item: " <> state.input) : state.items
                              , nextId = state.nextId + 1
                              })
        InputChange str ->
          modifyState (\state ->
                        state { input = str })

    spec this = do
      pure { render: renderer render this
           , state: initialState
           }
      where
        de = affAction this <<< eval
        (Dispatch1 d) = mkDispatch1 de
        render {state, props} =
          view_
             [ textInput'
                { onChange: d $ InputChange <<< _.nativeEvent.text
                , style: sheet.textInput
                , placeholder: "Enter item to log..."
                }
             , button "Add Log Item" $ d (const AddItem)
             , listView (listViewDataSource state.items) text_
             ]

type ItemsListProps = {}

type State =
  { nextId :: Int
  , items :: Array String
  , input :: String
  }

initialState :: State
initialState =
  { nextId: 0
  , items: []
  , input: ""
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
  , textInput :: Styles
}

sheet = {
  container: staticStyles [
      marginTop 64
    , padding 3
    , paddingLeft 8
    -- , flexDirection row
    , alignItems center
  ],
  textInput: staticStyles [
      fontSize 15
    , height 30
    -- , flex 1
  ]
}

main :: Effect Unit
main = do
  registerComponent "HealthTrack" app
