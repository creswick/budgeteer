{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Budgeteer.Types where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Maybe (listToMaybe)
import Data.Text
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.SqlQQ
import GHC.Generics

import Budgeteer.Db.Instances

type Name = Text

type Description = Maybe Text

type Cost = Int

data Lifetime = Lifetime NominalDiffTime
  deriving (Show, Ord, Eq)

data Item id = Item { itID :: id
                    , itName :: Name
                    , itDescription :: Description
                    , itReplacementDate :: UTCTime
                    , itReplacementCost :: Cost
                    } deriving (Read, Show, Eq, Ord, Generic)

instance Identifiable Item where
  setID theId item = item { itID = WithID theId }

instance ToJSON (Item WithID)
instance FromJSON (Item MaybeID)

instance FromRow (Item WithID) where
  fromRow = do itID <- field
               itName <- field
               itDescription <- field
               itReplacementDate <- field
               itReplacementCost <- field
               return Item {..}

instance ToRow (Item NoID) where
  toRow Item{..} = [ toField itName
                   , toField itDescription
                   , toField itReplacementDate
                   , toField itReplacementCost
                   ]

instance Loadable Item where
  loadByID conn theID = do
    items <- query conn [sql| SELECT i.item_id, it.name, i.description, i.replacement_date, i.replacement_cost
                             FROM item as i,item_template as it
                             WHERE i.item_template_id=it.item_template_id AND item_id = ?
                             |] (Only theID)
    return $ listToMaybe items

  loadAll conn =
    query_ conn [sql| SELECT i.item_id, it.name, i.description, i.replacement_date, i.replacement_cost
                 FROM item as i,item_template as it
                 WHERE i.item_template_id=it.item_template_id
                 |]


instance Storable Item where
  store conn item = do
    res <- query conn [sql| INSERT INTO item
                              (item_template_id, description, replacement_cost, replacement_date)
                              VALUES (?, ?, ?, ?)
                              RETURNING item_id
                         |] item
    case res of
      [] -> return $ Left ("Could not store item: "++show item)
      (Only newId:_) -> return $ Right $ setID newId item

data ItemTemplate id = ItemTemplate { itempID :: id
                                    , itempName :: Name
                                    , itempReplacementDate :: UTCTime
                                    , itempReplacementCost :: Cost
                                    }

instance FromRow (ItemTemplate WithID) where
  fromRow = do
    itempID <- field
    itempName <- field
    itempReplacementDate <- field
    itempReplacementCost <- field
    return ItemTemplate {..}


instance Identifiable ItemTemplate where
  setID theId it = it { itempID = WithID theId }
