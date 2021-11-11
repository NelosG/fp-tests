module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import GHC.Natural (Natural)

data Day =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool

-- | Computes the number of days until Friday.
daysToParty :: Day -> Natural
