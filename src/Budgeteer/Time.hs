module Budgeteer.Time

where

import Data.Time.Clock

seconds_in_hour :: Int
seconds_in_hour = 60 * 60

seconds_in_day :: Int
seconds_in_day = seconds_in_hour * 24

seconds_in_month :: Int
seconds_in_month = seconds_in_day * 30

seconds_in_year :: Int
seconds_in_year = seconds_in_day * 365

hours :: Int -> Int
hours = (seconds_in_hour *)

days :: Int -> Int
days = (seconds_in_day *)

years :: Int -> Int
years = (seconds_in_year *)

durationDays :: Int -> Int
durationDays diffTime = diffTime `div` seconds_in_day

durationMonths :: Int -> Int
durationMonths diffTime = diffTime `div` seconds_in_month

durationYears :: NominalDiffTime -> Int
durationYears diffTime = floor (diffTime / (fromIntegral seconds_in_year))
