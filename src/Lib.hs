module Lib ( printMonth, prettyPrintMonth ) where

import Data.List.Split
import Data.Time.Calendar
import Data.Time.Format
import System.Console.ANSI
import Text.PrettyPrint.Boxes
import Text.Printf
import Data.Bool

printMonth :: Day -> Integer -> Int -> IO ()
printMonth today y m = printBox $ prettyPrintMonth today y m

prettyPrintMonth :: Day -> Integer -> Int -> Box
prettyPrintMonth today y m =
  vcat top (title y m : dayOfWeekHeader : map (week today) weeks)
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

week :: Day -> [Day] -> Box
week _ [] = undefined
week today w@(d1:_) = moveRight (3 * leftOffset) $ hsep 1 left days
  where
    leftOffset = length [Monday .. dayOfWeek d1] - 1
    days = map (day today) w

day :: Day -> Day -> Box
day today d = text $ colorFunc $ printf "%2d" dayNum
  where
    (_, _, dayNum) = toGregorian d
    colorFunc = bool id blackOnWhite (d == today)

blackOnWhite :: String -> String
blackOnWhite s =
  concat
    [ setSGRCode [SetColor Background Vivid White]
    , setSGRCode [SetColor Foreground Vivid Black]
    , s
    , setSGRCode []
    ]
