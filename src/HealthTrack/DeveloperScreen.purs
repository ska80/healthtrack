module HealthTrack.DeveloperScreen where

import Prelude

import Effect (Effect)
import HealthTrack.Model (AppState, ppAppState)
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, scrollView, text, string)
import React.Basic.DOM (css)

type Props =
  { returnToMenuE :: Effect Unit
  , appState :: AppState
  }

comp :: Component Props
comp = createComponent "DeveloperScreen"

developerScreen :: Props -> JSX
developerScreen props' = makeStateless comp render props'
  where
    render props =
      scrollView { style: css { paddingTop: 70 }
           , children }
      where
        children =
          [ button { title: "< Menu"
                   , key: "MenuButton"
                   , onPress: capture_ props.returnToMenuE
                   }
          , text { children: [ string $ ppAppState props.appState ]
                 , key: "debugOutputTextArea"
                 }
          ]
