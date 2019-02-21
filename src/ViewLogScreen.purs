module ViewLogScreen where

import Data.Array (fromFoldable, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toMaybe)
import Effect (Effect)
import Effect.Console (log)
import Model (AppState, Screen(..))
import Prelude (Unit, discard, identity, show, ($), (+), (<<<))
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import React.Basic.Native (text, string, button, view, textInput, flatList)
import Unsafe.Coerce (unsafeCoerce)

comp :: Component Props
comp = createComponent "ViewLogScreen"

type Props =
  { returnToMenuE :: Effect Unit
  , state :: AppState
  }

viewLogScreen :: Props -> JSX
viewLogScreen props' = makeStateless comp render props'
  where
    render :: Props -> JSX
    render props =
      view { style: css {flexDirection: "column", padding: 100}
           , children:
             [ button { title: "< Menu"
                      , key: "MenuButton"
                      , onPress: capture_ props.returnToMenuE
                      }
             , flatList { data: unsafeCoerce $ fromFoldable props.state.items
                        , key: "itemsList"
                        , renderItem: unsafeCoerce renderItem
                        }
             , text { key: "testing", children: [ string "HEYA" ] }
             ]
           }
        where
          renderItem {item} =
            text { children: [ string item.val ] }

getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
