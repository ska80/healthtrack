module HealthTrack.Time where

import Prelude

import Data.Maybe (Maybe)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime, unInstant)
import Data.Time.Component (Millisecond)
import Data.DateTime as DT
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Time.Duration (Minutes(..), negateDuration, Milliseconds(..))
import Data.Enum (fromEnum)
import Data.Debug as D

newtype TZOffset = TZOffset Minutes

derive instance genericTZOffset :: Generic TZOffset _

instance showTZOffset :: Show TZOffset where
  show = genericShow

instance tzOffsetDebug :: D.Debug TZOffset where
  debug (TZOffset (Minutes mins))=
    let
      minutesRep = D.constructor "Mintues" [D.number mins]
    in
     D.constructor "TZOffset" [minutesRep]

newtype UTCInst = UTCInst Instant

derive instance genericUTCInst :: Generic UTCInst _

instance utcInstShow :: Show UTCInst where
  show = genericShow

instance utcInstDebug :: D.Debug UTCInst where
  debug (UTCInst instant)=
    let
      Milliseconds millis = unInstant instant
      numMilRep = D.constructor "Milliseconds" [D.number millis]
      instantRep = D.constructor "Instant" [numMilRep]
    in
     D.constructor "UTCInst" [instantRep]

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
