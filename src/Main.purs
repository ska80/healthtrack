module Main where
import Prelude
import Effect.Console
import Effect
import React.Basic (JSX)
import React.Basic.Native (text_) as RN
import Unsafe.Coerce (unsafeCoerce)

helloWorld :: JSX
helloWorld = RN.text_ [ unsafeCoerce "Hello, World!" ]

main :: Effect Unit
main =
  log "TODO remove me"
