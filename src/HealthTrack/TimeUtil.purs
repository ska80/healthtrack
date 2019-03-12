module HealthTrack.TimeUtil where

import Prelude

import Effect (Effect)
import HealthTrack.Time (TZOffset(..), adjustUTCForTimeZone, LocalInst(..), UTCInst)
import Data.JSDate  as JSD
import Data.Time.Duration (Minutes(..))
import Data.Formatter.DateTime as F
import Data.DateTime.Instant as Inst
import Data.List (fromFoldable)
import Data.Maybe (maybe)


getTZOffset :: Effect TZOffset
getTZOffset = do
  date <- JSD.now
  offset <- JSD.getTimezoneOffset date
  pure $ TZOffset $ Minutes $ offset

dateTimeFormatter :: F.Formatter
dateTimeFormatter =
  fromFoldable
  [ F.MonthShort
  , F.Placeholder " "
  , F.DayOfMonthTwoDigits
  , F.Placeholder ", "
  , F.YearFull
  , F.Placeholder " at "
  , F.Hours12
  , F.Placeholder ":"
  , F.MinutesTwoDigits
  , F.Placeholder " "
  , F.Meridiem
  ]

utcInstDisplayLocal :: TZOffset -> UTCInst -> String
utcInstDisplayLocal offset utcInst =
  let
    mLocalInst = adjustUTCForTimeZone offset utcInst
  in
   maybe "(Unable to convert time)" localInstDisplay mLocalInst

localInstDisplay :: LocalInst -> String
localInstDisplay (LocalInst instant) =
  F.format dateTimeFormatter $ Inst.toDateTime instant
