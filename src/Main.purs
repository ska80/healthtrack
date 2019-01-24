module Main where

import Prelude

import Effect (Effect)
import React (ReactClass, statelessComponent)
import ReactNative.API (registerComponent)
import ReactNative.Components.Text (text_)

app :: forall p. ReactClass {|p}
app = statelessComponent render
  where
    render _ = text_ "Hello World"

main :: Effect Unit
main = do
  registerComponent "HealthTrack" app
