module Lib ( printMonth, prettyPrintMonth ) where

import Data.List.Split
import Data.Time.Calendar
import Data.Time.Format
import System.Console.ANSI
import Text.PrettyPrint.Boxes
import Text.Printf

printMonth :: Day -> Integer -> Int -> IO ()
printMonth today y m = printBox $ prettyPrintMonth today y m

prettyPrintMonth :: Day -> Integer -> Int -> Box
prettyPrintMonth today y m =
  vcat top [title y m, dayOfWeekHeader, vcat top $ map (week today) weeks]
  where
    numDays = gregorianMonthLength y m
    days = map (fromGregorian y m) [1 .. numDays]
    weeks = sepByWeeks days

-- TODO: maybe make less ugly
sepByWeeks :: [Day] -> [[Day]]
sepByWeeks days = firstWeek : remainingWeeks
  where
    (firstWeekMinusSunday, sunday:remainingDays) = break isSunday days
    firstWeek = firstWeekMinusSunday ++ [sunday]
    remainingWeeks = chunksOf 7 remainingDays
    isSunday d = dayOfWeek d == Sunday

title :: Integer -> Int -> Box
title y m = alignHoriz center1 20 $ text (monthTitle ++ show y)
  where
    monthTitle = formatTime defaultTimeLocale "%B " $ fromGregorian y m 1

dayOfWeekHeader :: Box
dayOfWeekHeader =
  hsep 1 left $ map text ["Mo", "Tu", "We", "Th", "Fr", "Sa", "Su"]

week :: Day -> [Day] -> Box
week _ [] = undefined
week today w@(d1:_) = hsep 1 left days
  where
    startDay = dayOfWeek d1
    n = length [Monday .. startDay] - 1
    days = if startDay == Monday
      then map (day today) w
      else replicate n blankDay ++ map (day today) w

blankDay :: Box
blankDay = emptyBox 1 2

day :: Day -> Day -> Box
day today d = text $ colorFunc $ printf "%2d" dayNum
  where
    (_, _, dayNum) = toGregorian d
    colorFunc = if d == today
      then blackOnWhite
      else id

blackOnWhite :: String -> String
blackOnWhite s =
  concat
    [ setSGRCode [SetColor Background Vivid White]
    , setSGRCode [SetColor Foreground Vivid Black]
    , s
    , setSGRCode []
    ]
