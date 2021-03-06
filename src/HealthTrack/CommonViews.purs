module HealthTrack.CommonViews where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1)
import HealthTrack.Util as Util
import HealthTrack.Model (Item, ItemEntry)
import HealthTrack.ModelUtil as MU
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM.Events (capture_, capture)
import React.Basic.Events (SyntheticEvent)
import React.Basic.Native (button, textInput, view)

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


headerRowView :: Array JSX -> Array JSX -> JSX
headerRowView header body =
  let
    styles = { headerContWrap: css { flexDirection: "column"
                                   , padding: 50
                                   , width: "100%"
                                   , height: "100%"
                                   }
             , headerWrap: css { flexDirection: "row"
                               , justifyContent: "space-between"
                               }
             }
  in
   view { style: styles.headerContWrap
        , key:  "HeaderRowView"
        , children:
          [ view { style: styles.headerWrap
                 , key: "headerRowWrapper"
                 , children: header
                 }
          , view { key: "wrapperView", children: body }
          ]
        }

headerButtonsView ::
    { backButton ::
      { text :: String
      , action :: EffectFn1 SyntheticEvent Unit
      }
    , optionButton ::
      { text :: String
      , action :: EffectFn1 SyntheticEvent Unit
      }
    }
    -> Array JSX -> JSX
headerButtonsView props children =
  let
    header = [ wButton { title: "< " <> props.backButton.text
                       , key:  "backbutton"
                       , onPress: props.backButton.action
                       }
             , wButton { title: props.optionButton.text
                       , key:  "optionButton"
                       , onPress: props.optionButton.action
                       }
             ]
  in
   headerRowView header children

wButton :: { title :: String
           , key :: String
           , onPress :: EffectFn1 SyntheticEvent Unit
           } -> JSX
wButton props =
  view { key: props.key
       , style: css { margin: 20 }
       , children: [ button { title: props.title
                            , key: props.key
                            , onPress: props.onPress
                            }
                   ]
       }
