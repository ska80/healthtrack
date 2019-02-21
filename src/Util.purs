module Util where

foreign import jsonPrint_ :: forall a . a -> String

objToJSONString :: forall a . a -> String
objToJSONString = jsonPrint_
