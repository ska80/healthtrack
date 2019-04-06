module HealthTrack.Util where

-- module intended to have generic utilities, indpeendent of anything from HealtTrack

-- import Prelude

import React.Basic.Events (EventFn, SyntheticEvent, unsafeEventFn)
import Data.Maybe (Maybe)
import Data.Nullable (toMaybe)
import Unsafe.Coerce (unsafeCoerce)
import React.Basic.Native (FlatListPropsItemSeparatorComponent, ListRenderItem)
import React.Basic (JSX)

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
