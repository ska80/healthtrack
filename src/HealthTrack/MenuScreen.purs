module HealthTrack.MenuScreen where

import Prelude

import Effect (Effect)
import HealthTrack.Model (Screen(..))
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, view)

type Props
  = { onMenuClick :: Screen -> Effect Unit
    }

comp :: Component Props
comp = createComponent "MenuScreen"

menu :: Props -> JSX
menu props' = makeStateless comp render props'
  where
    render props =
      view { key: "menu"
           , style: css { flexDirection: "column", padding: 100 }
           , children: buttonElements
           }
      where
        buttonElements =
          [
            button { title: "Add Entry"
                   , key: "AddItemScreenButton"
                   , onPress: capture_ (props.onMenuClick AddItemScreen)
                   }
          , button { title: "View Entries"
                   , key: "ViewLogButton"
                   , onPress: capture_ (props.onMenuClick ListItemEntriesScreen)
                   }
          , button { title: "Developer"
                   , key: "DeveloperButton"
                   , onPress: capture_ (props.onMenuClick DeveloperScreen)
                   }
          ]
