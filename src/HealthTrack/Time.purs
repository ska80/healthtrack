module HealthTrack.Time where

import Prelude

import Data.Maybe (Maybe)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.DateTime as DT
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Time.Duration (Minutes, negateDuration)

newtype TZOffset = TZOffset Minutes

derive instance genericTZOffset :: Generic TZOffset _

instance showTZOffset :: Show TZOffset where
  show = genericShow

newtype UTCInst = UTCInst Instant

derive instance genericUTCInst :: Generic UTCInst _

instance createdAtInstShow :: Show UTCInst where
  show = genericShow


newtype LocalInst = LocalInst Instant

adjustUTCForTimeZone :: TZOffset -> UTCInst -> Maybe LocalInst
adjustUTCForTimeZone (TZOffset mintues) (UTCInst instant) =
  -- unclear to me why offset needs to be negated and doens't come from
  -- browser as negative, but hey, what do I know?
  let
    instAsDateTime = toDateTime instant
    adjusted = DT.adjust (negateDuration mintues) instAsDateTime
  in
   (LocalInst <<< fromDateTime) <$> adjusted
