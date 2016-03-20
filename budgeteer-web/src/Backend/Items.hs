module Backend.Items where

import Database.PostgreSQL.Simple (Connection)

import Budgeteer.Types
import Budgeteer.Db.Instances

-- | Store a new item, assigning it an ID, and returning the
-- Identified Item on success.
addItem :: Item NoID -> Connection -> IO (Either Error (Item WithID))
addItem item conn = store conn item
