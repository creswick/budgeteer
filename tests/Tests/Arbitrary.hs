{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tests.Arbitrary

where

import Control.Applicative (pure)
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Budgeteer.Types
import Budgeteer.Db.Instances

instance Arbitrary (Item NoID) where
  arbitrary = do
    itID <- pure NoID
    itName <- arbitrary
    itDescription <- arbitrary
    itReplacementDate <- arbitrary
    itReplacementCost <- arbitrary
    return Item {..}

