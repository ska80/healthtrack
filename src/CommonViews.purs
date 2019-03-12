module CommonViews where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM.Events (capture_)
import React.Basic.Native (button)

returnToMenuButton :: forall r . { returnToMenuE :: Effect Unit | r } -> JSX
returnToMenuButton props =
  button { title: "< Menu"
         , key: "MenuButtonComp"
         , onPress: capture_ props.returnToMenuE
         }
