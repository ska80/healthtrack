module MenuScreen where

import React.Basic (JSX, Component, makeStateless, createComponent)

import Model (Screens(..))
import Effect (Effect)
import Prelude (Unit)

import React.Basic.Native (button, view)
import React.Basic.DOM.Events (capture_)
import React.Basic.DOM (css)

type Props
  = { onMenuClick :: Screens -> Effect Unit
    }

comp :: Component Props
comp = createComponent "MenuScreen"

menu :: Props -> JSX
menu props' = makeStateless comp render props'
  where
    render props =
      view { key: "debugLabelInfoEmpty"
           , style: css { flexDirection: "column", padding: 100 }
           , children: buttonElements
           }
      where
        buttonElements =
          [ button { title: "Add Item"
                   , key: "AddItemScreenButton"
                   , onPress: capture_ (props.onMenuClick AddItemScreen)
                   }
          , button { title: "Developer"
                   , key: "DeveloperButton"
                   , onPress: capture_ (props.onMenuClick DeveloperScreen)
                   }
          ]
