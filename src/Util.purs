module Util where

import Prelude

foreign import jsonPrint_ :: forall a . a -> String

-- TODO figure out better type for this than `String`?
objToJSONString :: forall a . a -> String
objToJSONString = jsonPrint_
