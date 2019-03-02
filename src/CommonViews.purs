module CommonViews where

import Prelude

import Effect (Effect)
import React.Basic (JSX, Component, makeStateless, createComponent)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button)

-- type Props = { returnToMenuE :: Effect Unit }

-- returnToMenuButton :: Props -> JSX
returnToMenuButton :: { returnToMenuE :: Effect Unit } -> JSX
returnToMenuButton props =
  button { title: "< Menu"
         , key: "MenuButtonComp"
         , onPress: capture_ props.returnToMenuE
         }
