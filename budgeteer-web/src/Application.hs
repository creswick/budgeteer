{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.Reader ( local )
import Control.Monad.State ( get )
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
import API.Meta (Meta(..))
------------------------------------------------------------------------------

data App = App
  { _sess :: Snaplet SessionManager
  , _auth :: Snaplet (AuthManager App)
  , _db   :: Snaplet Postgres
  , _meta :: Snaplet Meta
  }

makeLenses ''App

instance HasPostgres (Handler b App) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

------------------------------------------------------------------------------
type AppHandler = Handler App App


