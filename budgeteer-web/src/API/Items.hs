{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module API.Items
  ( Items(..)
  , itemsInit
  )
where

import           Control.Lens
import           Control.Monad.Reader ( local )
import           Control.Monad.State ( get )
import qualified Data.ByteString.Char8 as C8
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

import           Utils.WebUtils (writeJSON)

data Items = Items {
  _db   :: Snaplet Postgres
  }
makeLenses ''Items

instance HasPostgres (Handler b Items) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)


itemsInit :: SnapletInit b Items
itemsInit = makeSnaplet "items" "Items Api" Nothing $ do
      d <- nestSnaplet "db" db pgsInit
      addRoutes itemsRoutes
      return (Items d)

itemsRoutes :: [(C8.ByteString, Handler b Items ())]
itemsRoutes = [ ("", method GET handleListItems)
              , (":itemid", method GET handleGetItem)
              ]

handleListItems :: Handler b Items ()
handleListItems = modifyResponse $ setResponseCode 200 -- TODO

handleGetItem :: Handler b Items ()
handleGetItem = modifyResponse $ setResponseCode 200 -- TODO
