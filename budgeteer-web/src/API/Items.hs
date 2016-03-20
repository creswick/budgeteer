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
import           Snap.Extras.JSON (getJSON)
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple

import           Budgeteer.Db.Instances
import           Budgeteer.Types (Item(..))

import qualified Utils.WebUtils as Web
import           Utils.DBUtils (withPGsql)

data Items = Items { _db   :: Snaplet Postgres
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
itemsRoutes = [ ("", method GET handleListItems <|> method POST handleAddItem )
              , (":itemid", method GET $ Web.getIntParam "itemid" handleGetItem)
              ]

handleListItems :: Handler b Items ()
handleListItems = do
  is <- withPGsql loadAll
  Web.writeJSON (is :: [Item WithID])

handleGetItem :: Int -> Handler b Items ()
handleGetItem theID = do
  mItem <- withPGsql (\c -> loadByID c (ID theID))
  case mItem of
    Nothing   -> Web.badRequest (C8.pack ("No item found with id: " ++ show theID))
    Just item -> Web.writeJSON (item :: Item WithID)

handleAddItem :: Handler b Items ()
handleAddItem = do
  eItem <- getJSON
  case eItem of
    Left err -> Web.badRequest (C8.pack ("Invalid Item encoding: "++err)) -- TODO Wrong response code?
    Right item -> do
       eRes <- withPGsql (addItem item)
       case eRes of
         Left   err -> Web.badRequest (C8.pack ("Could not store new item: "++err))
         Right item -> Web.writeJSON (item :: Item WithID)
