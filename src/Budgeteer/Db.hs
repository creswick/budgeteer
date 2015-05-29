module Budgeteer.Db where


import Database.PostgreSQL.Simple

import Budgeteer.Types


readItems :: Connection -> IO [Item]
readItems conn = query_ "SELECT * from ITEM"
