module HealthTrack.DeveloperScreen where

import Prelude

import Effect (Effect)
import HealthTrack.Model (AppState, ppAppState)
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, view, text, string)
import HealthTrack.AutoComplete (autoComplete, Action(..), Entry)
import HealthTrack.AutoComplete as AC
import React.Basic.DOM (css)
import Data.String as Str
import Data.List as List
import Data.List (List)
import Data.Maybe (maybe)
import Data.Array (mapWithIndex)

symptoms :: List AC.Entry
symptoms = fixupInitialEntries
  [ "headache"
  , "stomach pain"
  , "joint pain - back"
  , "joint pain - hands"
  ]

fixupInitialEntries :: Array String -> List Entry
fixupInitialEntries =
  List.fromFoldable <<< mapWithIndex toEntry
  where
    toEntry id val = { key: show id, val }

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
          [ button { title: "< Menu"
                   , key: "MenuButton"
                   , onPress: capture_ props.returnToMenuE
                   }
          , text { children: [ string $ ppAppState props.appState ]
                 , key: "debugOutputTextArea"
                 }
          ]
