module Main where

import Data.Time.Calendar
import Data.Time.LocalTime
import Options.Applicative

import Lib

main :: IO ()
main = do
  today <- getCurrentDay
  let (thisYear, thisMonth, _) = toGregorian today
      argParser = (,) <$> (yearP thisYear) <*> (monthP thisMonth)
      parser = info (argParser <**> helper) (fullDesc <> progDesc "Print a calendar")
  (y, m) <- execParser parser
  printMonth today y m

yearP :: Integer -> Parser Integer
yearP y = option auto
  (  long "year"
  <> short 'y'
  <> metavar "Y"
  <> value y
  <> help "Print Y year (defaults to current year)" )

monthP :: Int -> Parser Int
monthP m = option auto
  (  long "month"
  <> short 'm'
  <> metavar "M"
  <> value m
  <> help "Print M month (defaults to current month)" )

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime
