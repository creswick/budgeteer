{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Budgeteer.Items

where

import Data.Time.Clock
import Data.Text (Text)
import qualified Data.Text as T

import Budgeteer.Time

newtype Name = Name Text
  deriving (Read, Show, Ord, Eq)

newtype Description = Desc Text
  deriving (Read, Show, Ord, Eq)

newtype Cost = Cost Int
  deriving (Read, Show, Ord, Eq)

-- newtype Durability = Dur Int
--   deriving (Read, Show, Ord, Eq)

newtype Wear = Wear Rational
  deriving (Read, Show, Ord, Eq)

-- | How much does the wear of an item change over a given period?
newtype WearRate = WearRate Rational
  deriving (Read, Show, Ord, Eq)

newtype Lifetime = Lifetime NominalDiffTime
  deriving (Show, Ord, Eq)

data Item = Item { name :: Name
                 , description :: Maybe Description
                 , initCost :: Cost
                 , newDate :: UTCTime
                 , replacementDate :: UTCTime
                 , replacementCost :: Cost
--                 , durability :: Durability
                 , wearRate :: WearRate
                 , temporalWearRate :: WearRate
                 , wearState :: Wear
                 , wearAxis :: Text
                 , lifetimeEstimate :: Lifetime
                 } deriving (Eq, Ord)

investNow :: Item -> Cost
investNow item = calcInvestment (replacementCost item) 0.08 (lifetimeEstimate item)

investMonthly :: Item -> Cost
investMonthly item = calcMonthInvestment (replacementCost item) 0.08 (lifetimeEstimate item)

mkItem :: Text -- ^ The name of the item
       -> Double -- ^ Cost, in dollars.
       -> Int    -- ^ Lifetime, in seconds
       -> IO Item
mkItem name cost lifetime =
  newItem (Name name) (Cost $ floor $ 100 * cost) (Lifetime $ fromIntegral $ lifetime)

newItem :: Name -> Cost -> Lifetime -> IO Item
newItem name cost (Lifetime lifetime) = do
  let description = Nothing
      initCost = cost
  newDate <- getCurrentTime
  let replacementDate = addUTCTime lifetime newDate
      replacementCost = calcReplacementCost initCost (Lifetime lifetime)
      wearRate = WearRate 1 -- default wear for ageing
      temporalWearRate = wearRate
      wearState = Wear 1.0
      wearAxis = "Age"
      lifetimeEstimate = (Lifetime lifetime)
  return Item { .. }


inflation_rate :: Double
inflation_rate = 0.01 -- 1%?

-- A = P (1 + r/n) ^ nt:
-- Where:
-- A = the future value of the investment/loan, including interest
-- P = the principal investment amount (the initial deposit or loan amount)
-- r = the annual interest rate (decimal)
-- n = the number of times that interest is compounded per year
-- t = the number of years the money is invested or borrowed for
calcReplacementCost :: Cost -> Lifetime -> Cost
calcReplacementCost (Cost p) (Lifetime lifetime) =
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

  in Cost (floor theCost)


-- P = A / ( 1 + r/n ) ^ nt.
--
-- assumes monthly compounding
calcInvestment :: Cost     -- ^ amount you need
               -> Double   -- ^ interest rate
               -> Lifetime -- ^ when you need it
               -> Cost     -- ^ how much to invest now
calcInvestment (Cost a) r (Lifetime lifetime) =
  let t = fromIntegral $ durationYears lifetime

      n = fromIntegral 12 -- monthly compounding

      total = (fromIntegral a) / ((1 + r / n) ** (n * t))

  in Cost (floor total)


-- A = PMT * (((1 + r/n)^nt - 1) / (r/n))
-- PMT = A / (((1 + r/n)^nt - 1) / (r/n))
--
-- assumes monthly compounding
calcMonthInvestment :: Cost     -- ^ amount you need
               -> Double   -- ^ interest rate
               -> Lifetime -- ^ when you need it
               -> Cost     -- ^ how much to invest now
calcMonthInvestment (Cost a) r (Lifetime lifetime) =
  let t = fromIntegral $ durationYears lifetime

      n = fromIntegral 12 -- monthly compounding

      monthly = (fromIntegral a) / (((1 + r/n) ** (n * t) - 1) / (r/n))

  in Cost (floor monthly)

