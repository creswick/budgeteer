module Budgeteer.Types where

import Data.Text
import Data.Time.Clock

newtype ID = ID Int
  deriving (Read, Show, Eq, Ord)

type Name = Text

type Description = Text

type Cost = Int

data Lifetime = Lifetime NominalDiffTime
  deriving (Show, Ord, Eq)

data Item = Item { itId :: Maybe ID
                 , itName :: Name
                 , itReplacementDate :: UTCTime
                 , itReplacementCost :: Cost
                 } deriving (Eq, Ord)

data ItemTemplate = ItemTemplate { itempId :: Maybe ID
                                 , itempName :: Name
                                 , itempReplacementDate :: UTCTime
                                 , itempReplacementCost :: Cost
                                 }

