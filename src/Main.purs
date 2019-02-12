module Main where
import Prelude (($), (+), (<>), show)
-- import Effect.Console
-- import Effect
import React.Basic (StateUpdate(..), JSX, make, runUpdate, Component, createComponent)
import React.Basic.Native (text_, string, view_, button)
import React.Basic.DOM.Events (capture_)
-- import Unsafe.Coerce (unsafeCoerce)

comp :: Component {}
comp = createComponent "Main"

data Action
  = Increment

-- main :: RB.JSX
-- main = RN.text_ [ RN.string "hey" ]

main :: JSX
main = make comp
  { render, initialState: {count: 0} } {}
  where
    update self =
      case _ of
        Increment ->
          Update $ self.state { count = self.state.count + 1 }

    send = runUpdate update

    -- render :: Self {} State Action -> JSX
    render self =
      view_
        [ text_ [ string $ "hey " <> show self.state.count ]
        , button { title: "clicky"
                 , onPress: (capture_ $ send self Increment )
                 }
        ]
