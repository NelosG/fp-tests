module HW1.T1
       ( Day (..),
       nextDay,
       afterDays,
       isWeekend,
       daysToParty
       ) where

import GHC.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day


-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool



-- | Computes the number of days until the next Friday.
daysToParty :: Day -> Natural
