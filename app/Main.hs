module Main where

import Data.Time.Calendar
import Data.Time.LocalTime
import Options.Applicative

import Lib

main :: IO ()
main = do
  today <- getCurrentDay
  let (y, m, _) = toGregorian today
      argParser = Args <$> (monthP m) <*> (yearP y)
      parser = info (argParser <**> helper) (fullDesc <> progDesc "Print a calendar")
  Args m y <- execParser parser
  printMonth today y m

data Args =
  Args
    { month :: Int
    , year :: Integer
    }
  deriving (Show)

yearP :: Integer -> Parser Integer
yearP y = option auto ( long "year" <> short 'y' <> metavar "Y" <> showDefault <> value y <> help "Print Y year")

monthP :: Int -> Parser Int
monthP m = option auto ( long "month" <> short 'm' <> metavar "M" <> showDefault <> value m <> help "Print M month" )

getCurrentDay :: IO Day
getCurrentDay = localDay . zonedTimeToLocalTime <$> getZonedTime
