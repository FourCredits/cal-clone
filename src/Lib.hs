module Lib ( printMonth ) where

import Data.Time.Calendar
import Data.Time.Format
import Data.Time.LocalTime
import System.Console.ANSI
import Text.Printf

printMonth :: IO ()
printMonth = do
  now <- today
  let (y, m, d)       = toGregorian now
      month           = monthFullName now
      numDays         = gregorianMonthLength y m
      firstDayOfMonth = dayOfWeek $ fromGregorian y m 1
  putStrLn $ centreAlignTitle month y
  putStrLn "Mo Tu We Th Fr Sa Su"
  putStr $ offsetFirstWeek firstDayOfMonth
  putStrLn $ concatMap (formatDay now) [1 .. numDays]

today :: IO Day
today = localDay . zonedTimeToLocalTime <$> getZonedTime

monthFullName :: Day -> String
monthFullName day = formatTime defaultTimeLocale "%B" day

offsetFirstWeek :: DayOfWeek -> String
offsetFirstWeek day = replicate (3 * (length [Monday .. day] - 1)) ' '

isSunday :: Day -> Bool
isSunday d = dayOfWeek d == Sunday

formatDay :: Day -> Int -> String
formatDay today dayToFormat = colorFunc (printf "%2d" dayToFormat) ++ ending
  where
    (y, m, d) = toGregorian today
    colorFunc
      | d == dayToFormat = blackOnWhite
      | otherwise = id
    ending
      | isSunday $ fromGregorian y m dayToFormat = "\n"
      | otherwise = " "

centreAlignTitle :: String -> Integer -> String
centreAlignTitle month year = front ++ printf "%s %d" month year
  where
    n = (15 - length month) `div` 2
    front = replicate n ' '

blackOnWhite :: String -> String
blackOnWhite s =
  concat
    [ setSGRCode [SetColor Background Vivid White]
    , setSGRCode [SetColor Foreground Vivid Black]
    , s
    , setSGRCode []
    ]
