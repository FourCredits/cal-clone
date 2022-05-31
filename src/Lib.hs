module Lib ( printMonths ) where

import Data.List.Split
import Data.Time.Calendar
import Data.Time.Format
import Text.PrettyPrint.Boxes
import Text.Printf
import Data.Bool

type Month = (Integer, Int)

printMonths :: Month -> Int -> IO ()
printMonths (y, m) n = printBox $ hsep 5 left months
  where
    months = map (prettyPrintMonth . wrapMonth) [m .. (m + n - 1)]
    wrapMonth m = if m <= 12
      then (y, m)
      else (y + fromIntegral (m `div` 12), m `mod` 12)

prettyPrintMonth :: Month -> Box
prettyPrintMonth (y, m) =
  vcat top (title y m : dayOfWeekHeader : map week weeks)
  where
    numDays = gregorianMonthLength y m
    days = map (fromGregorian y m) [1 .. numDays]
    weeks = split (dropBlanks $ keepDelimsR $ whenElt isSunday) days
    isSunday d = dayOfWeek d == Sunday

title :: Integer -> Int -> Box
title y m = alignHoriz center1 20 $ text (monthTitle ++ show y)
  where monthTitle = formatTime defaultTimeLocale "%B " $ fromGregorian y m 1

dayOfWeekHeader :: Box
dayOfWeekHeader =
  hsep 1 left $ map text ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"]

week :: [Day] -> Box
week [] = undefined
week w@(d1:_) = emptyBox 1 (3 * leftOffset) Text.PrettyPrint.Boxes.<> hsep 1 left days
  where
    leftOffset = length [Monday .. dayOfWeek d1] - 1
    days = map day w

day :: Day -> Box
day d = text $ printf "%2d" dayNum
  where (_, _, dayNum) = toGregorian d
