module Utils.DBUtils
  ( withPGsql )
where

import           Database.PostgreSQL.Simple (Connection)
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple



-- | Provide the given action with a database connection (and then run
-- it synchronously)
withPGsql :: HasPostgres m => (Connection -> IO b) -> m b
withPGsql f = liftPG f
