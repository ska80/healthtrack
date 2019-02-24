module ViewLogScreen where

import Data.Array (fromFoldable)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Model (AppState, Screen(..))
import Prelude (Unit, ($))
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import React.Basic.Native (text, string, button, view, flatList)
import Unsafe.Coerce (unsafeCoerce)

comp :: Component Props
comp = createComponent "ViewLogScreen"

type Props =
  { returnToMenuE :: Effect Unit
  , state :: AppState
  , changeScreen :: Screen -> Effect Unit
  }

-- TODO finish add "new entry" button
viewLogScreen :: Props -> JSX
viewLogScreen props' = makeStateless comp render props'
  where
    render :: Props -> JSX
    render props =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             -- TODO add this "< Menu" button everywehre
             -- TODO extact menu button into some kind of common views module
             [ button { title: "< Menu"
                      , key: "MenuButton"
                      , onPress: capture_ props.returnToMenuE
                      }

             , button { title: "Add New Entry"
                      , key: "AddItemScreenButton"
                      , onPress: capture_ (props.changeScreen AddItemScreen)
                      }

             , flatList { data: unsafeCoerce $ fromFoldable props.state.items
                        , key: "itemsList"
                        , renderItem: unsafeCoerce renderItem
                        }
             ]
           }
        where
          renderItem {item} =
            text { children: [ string item.val ] }

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
