{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Budgeteer.Items

where

import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T

import Budgeteer.Time
import Budgeteer.Types
import Budgeteer.Db.Instances

interestRate :: Double
interestRate = 0.04

inflation_rate :: Double
inflation_rate = 0.03 -- 1%?

investNow :: Item id -> IO Cost
investNow item = do
  now <- getCurrentTime
  let timeToSave = Lifetime $ diffUTCTime (itReplacementDate item) now
  return $ calcInvestment (itReplacementCost item) interestRate timeToSave

investMonthly :: Item id -> IO Cost
investMonthly item = do
  now <- getCurrentTime
  let timeToSave = Lifetime $ diffUTCTime (itReplacementDate item) now
  return $ calcMonthInvestment (itReplacementCost item) interestRate timeToSave

mkItem :: Text -- ^ The name of the item
       -> Double -- ^ Cost, in dollars. Assumed to be a cost today.
       -> Int    -- ^ Lifetime, in seconds. Assumed to start from now.
       -> IO (Item NoID)
mkItem name cost lifetime = do
  now <- getCurrentTime
  let replacement = addUTCTime (fromIntegral lifetime) now
      baseCost = floor cost
  return Item { itID = NoID
              , itName = name
              , itDescription = Nothing
              , itReplacementCost = calcReplacementCost baseCost (Lifetime $ fromIntegral lifetime)
              , itReplacementDate = replacement
              }

calcReplacementCost :: Cost -> Lifetime -> Cost
calcReplacementCost p (Lifetime lifetime) =
  let
    theCost :: Double
    theCost = (fromIntegral p) * ((1.0 + inflation_rate) ** (fromIntegral $ durationYears lifetime))
  in floor theCost

-- A = P (1 + r/n) ^ nt:
-- Where:
-- A = the future value of the investment/loan, including interest
-- P = the principal investment amount (the initial deposit or loan amount)
-- r = the annual interest rate (decimal)
-- n = the number of times that interest is compounded per year
-- t = the number of years the money is invested or borrowed for
fv :: Cost -> Lifetime -> Cost
fv p (Lifetime lifetime) =
  let t :: Int
      t = durationYears lifetime

      r :: Double
      r = inflation_rate

      n :: Int
      n = 1

      r_by_n :: Double
      r_by_n = r / (fromIntegral n)

      theCost :: Double
      theCost = (fromIntegral p) * (( 1.0 + r_by_n) ** (fromIntegral n * fromIntegral t))

  in floor theCost


-- P = A / ( 1 + r/n ) ^ nt.
--
-- assumes monthly compounding
calcInvestment :: Cost     -- ^ amount you need
               -> Double   -- ^ interest rate
               -> Lifetime -- ^ when you need it -- time from now.
               -> Cost     -- ^ how much to invest now
calcInvestment a r (Lifetime lifetime) =
  let t = fromIntegral $ durationYears lifetime

      n = fromIntegral 12 -- monthly compounding

      total = (fromIntegral a) / ((1 + r / n) ** (n * t))

  in floor total


-- A = PMT * (((1 + r/n)^nt - 1) / (r/n))
-- PMT = A / (((1 + r/n)^nt - 1) / (r/n))
--
-- assumes monthly compounding
calcMonthInvestment :: Cost     -- ^ amount you need
                    -> Double   -- ^ interest rate
                    -> Lifetime -- ^ when you need it -- time from now
                    -> Cost     -- ^ how much to invest now
calcMonthInvestment a r (Lifetime lifetime) =
  let t = fromIntegral $ durationYears lifetime

      n = fromIntegral 12 -- monthly compounding

      monthly = (fromIntegral a) / (((1 + r/n) ** (n * t) - 1) / (r/n))

  in floor monthly

