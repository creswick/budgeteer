{-# LANGUAGE OverloadedStrings #-}
module API.Meta
  ( Meta(..)
  , metaInit
  )
where

import           Snap.Snaplet
import           Snap.Core
import qualified Data.ByteString.Char8 as C8

data Meta = Meta

metaInit :: SnapletInit b Meta
metaInit = makeSnaplet "metadata" "Metadata Api" Nothing $ do
             addRoutes metaRoutes
             return Meta

metaRoutes :: [(C8.ByteString, Handler b Meta ())]
metaRoutes = [("version", method GET handleVersion)]

handleVersion :: Handler b Meta ()
handleVersion = modifyResponse $ setResponseCode 200 -- TODO 
