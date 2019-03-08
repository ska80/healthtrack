module HealthTrack.Time where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..))
import Data.DateTime.Instant (Instant, instant, unInstant)
import Data.Time.Duration (Milliseconds(..))

import Data.DateTime.Instant as Inst
import Data.DateTime as DT
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to)
import Data.Generic.Rep.Show (genericShow)
import Data.Time.Duration (Minutes(..), negateDuration)

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
    instAsDateTime = Inst.toDateTime instant
    adjusted = DT.adjust (negateDuration mintues) instAsDateTime
  in
   (LocalInst <<< Inst.fromDateTime) <$> adjusted
