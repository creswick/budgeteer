{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Budgeteer.Db.Instances where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics (Generic)

type Error = String

-- | Things that can be retrieved from the database.
class Loadable a where
  loadByID :: Connection -> ID -> IO (Maybe (a WithID))
  loadAll :: Connection -> IO [a WithID]

-- | Storing fills in Maybe ID fields.
class Storable a where
  store :: Connection -> a NoID -> IO (Either Error (a WithID))

  storeAll :: Connection -> [a NoID] -> IO [Either Error (a WithID)]
  storeAll conn xs = mapM (store conn) xs

class Identifiable f where
  setID :: ID -> f NoID -> f WithID

newtype ID = ID Int
          deriving (Read, Show, Eq, Ord, FromField, ToField, ToJSON, FromJSON)

data NoID = NoID
            deriving (Eq, Ord, Read, Show, Generic)

data MaybeID = HasNoID
             | HasID ID
               deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON MaybeID
instance FromJSON MaybeID

newtype WithID = WithID { getID :: ID }
              deriving (Eq, Ord, Read, Show, Generic, FromField, ToField)

instance ToJSON WithID
instance FromJSON WithID
