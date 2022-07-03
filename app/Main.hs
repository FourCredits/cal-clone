module Main where

import Data.Time.Calendar
import Data.Time.LocalTime
import Options.Applicative

import Lib

main :: IO ()
main = do
  today <- getCurrentDay
  let (thisYear, thisMonth, _) = toGregorian today
      argParser = (,,) <$> yearP thisYear <*> monthP thisMonth <*> numberP
      parser = info (argParser <**> helper) (fullDesc <> progDesc "Print a calendar")
  (y, m, n) <- execParser parser
  printMonths (y, m) n

yearP :: Integer -> Parser Integer
yearP y = option auto
  (  long "year"
  <> short 'y'
  <> metavar "Y"
  <> value y
  <> help "Start from Y year (default: current year)" )

monthP :: Int -> Parser Int
monthP m = option auto
  (  long "month"
  <> short 'm'
  <> metavar "M"
  <> value m
  <> help "Start from M month (default: current month)" )

numberP :: Parser Int
numberP = option auto
  (  long "number"
  <> short 'n'
  <> metavar "N"
  <> value 1
  <> showDefault
  <> help "How many months to print" )

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime
