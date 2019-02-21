module DeveloperScreen where

import Effect (Effect)
import Model (AppState)
import Prelude (Unit, ($))
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button, view, text_, string)
import Util as Util

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
          [ button { title: "< Menu"
                   , key: "MenuButton"
                   , onPress: capture_ props.returnToMenuE
                   }
          , text_ [ string $ Util.objToJSONString props.appState ]
          ]
