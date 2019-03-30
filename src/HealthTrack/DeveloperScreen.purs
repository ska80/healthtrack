module HealthTrack.DeveloperScreen where

import Prelude

import Effect (Effect)
import HealthTrack.Model (AppState, ppAppState)
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, view, text, string)
import HealthTrack.AutoComplete (autoComplete)

symptoms :: Array String
symptoms =
  [ "headache"
  , "stomach pain"
  , "joint pain - back"
  , "joint pain - hands"
  , "joint pain - hands"
  ]


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
      view { children }
      where
        children =
          [ text { children: [ string $ "enter symptom name:" ]
                 , key: "debugOutputTextArea"
                 }

          , view {key: "yerp",
                  children: [
                    autoComplete { onEntryComplete: \x-> pure unit
                                 , key: "foo"
                                 , initialEntries: symptoms
                                 }
                    ]
                 }
          -- ,
          --   button { title: "< Menu"
          --          , key: "MenuButton"
          --          , onPress: capture_ props.returnToMenuE
          --          }

          -- , text { children: [ string $ ppAppState props.appState ]
          --        , key: "debugOutputTextArea"
          --        }
          ]
