module HealthTrack.CommonViews where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Native (button, textInput)
import HealthTrack.Util as Util
import Data.Maybe (Maybe, maybe)

returnToMenuButton :: forall r . { returnToMenuE :: Effect Unit | r } -> JSX
returnToMenuButton props =
  button { title: "< Menu"
         , key: "MenuButtonComp"
         , onPress: capture_ props.returnToMenuE
         }

notesInput :: Maybe String -> (Maybe String -> Effect Unit) -> Effect Unit -> JSX
notesInput value onChange onSubmit =
  textInput { key: "txtinput"
            , placeholder: "Enter note text here"
            , style: css { flex: 1
                         , borderWidth: 1
                         , borderColor: "black"
                         , padding: 5
                         , width: "100%"
                         }
            , onChange: (capture Util.getText onChange)
            , value: maybe "" identity value
            , onSubmitEditing: (capture_ $ onSubmit)
              -- TODO maybe reenable autocorrect? seems like there
              -- should be a better way to fix the weird way the
              -- app was re-populating the field. idk.
            , autoCorrect: false
            , multiline: true
            }
