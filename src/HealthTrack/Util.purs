module HealthTrack.Util where

-- module intended to have generic utilities, indpeendent of anything from HealtTrack

import Prelude


import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Data.String as Str
import React.Basic (JSX)
import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import React.Basic.Native (FlatListPropsItemSeparatorComponent, ListRenderItem, KeyboardAvoidingViewPropsBehavior)
import Unsafe.Coerce (unsafeCoerce)
foreign import jsonPrint_ :: forall a . a -> String

objToJSONString :: forall a . a -> String
objToJSONString = jsonPrint_

-- use getText to extract text from a textinput change event
getText :: EventFn SyntheticEvent (Maybe String)
getText = unsafeEventFn \e ->
  toMaybe (unsafeCoerce e).nativeEvent.text

toListRenderItem :: forall a . (a -> JSX) -> ListRenderItem
toListRenderItem = unsafeCoerce

toFlatListPropsItemSeparatorComponent :: ({ highlighted :: Boolean } -> JSX)
                                         -> FlatListPropsItemSeparatorComponent
toFlatListPropsItemSeparatorComponent = unsafeCoerce

toKbdAvdPropBehv :: String -> KeyboardAvoidingViewPropsBehavior
toKbdAvdPropBehv = unsafeCoerce

stringHasContent :: String -> Boolean
stringHasContent s = Str.length s > 0

filterEmptyStrings :: Array String -> Array String
filterEmptyStrings = Array.filter stringHasContent
