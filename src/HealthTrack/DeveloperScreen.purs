module HealthTrack.DeveloperScreen where

import Prelude

import Effect (Effect)
import HealthTrack.Model (AppState, ppAppState)
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, view, text, string)
import HealthTrack.AutoComplete (autoComplete, Action(..), Entry)
import React.Basic.DOM (css)

symptoms :: Array String
symptoms =
  [ "headache"
  , "stomach pain"
  , "joint pain - back"
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
      view { style: css { paddingTop: 70 }
           , children }
      where
        children =
          [
            button { title: "< Menu"
                   , key: "MenuButton"
                   , onPress: capture_ props.returnToMenuE
                   }
          ,
            text { children: [
                      string $ "enter symptom name, "
                      ]
                 , key: "debugOutputTextArea"
                 }
          ,
            text { children: [
                      string $ "or select from list:"
                      ]
                 , key: "debugOutputTextArea2"
                 }
          ,
            view {
               key: "testing automcomp key"
               ,
               style: css {
                 height: "100%"
                 ,
                 width: "100%"
                 -- ,
                 -- borderColor: "red"
                 -- ,
                 -- borderWidth: 1
                 }
               ,
               children: [
                 autoComplete { onEntryComplete: \x-> pure unit
                              , key: "foo"
                              , initialEntries: symptoms
                              , addCreateEntry: true
                              , handler: eventHandler
                              -- , userState: unit
                              }
                 ]

               }
          ]

        -- eventHandler ::
        eventHandler act =
          pure unit
