module Budgeteer.Db where


import Database.PostgreSQL.Simple

import Budgeteer.Types
import Budgeteer.Db.Instances

readItems :: Connection -> IO [Item WithID]
readItems conn = loadAll conn

writeItems :: Connection -> [Item NoID] -> IO [Either Error (Item WithID)]
writeItems conn items = storeAll conn items
