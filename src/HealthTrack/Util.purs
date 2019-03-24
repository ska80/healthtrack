module HealthTrack.Util where

-- import Prelude

import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Unsafe.Coerce (unsafeCoerce)

foreign import jsonPrint_ :: forall a . a -> String

objToJSONString :: forall a . a -> String
objToJSONString = jsonPrint_

-- use getText to extract text from a textinput change event
getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text
