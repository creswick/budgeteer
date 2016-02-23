{-# LANGUAGE OverloadedStrings #-}
module Utils.WebUtils

where

import Data.Aeson
import Snap.Snaplet (Handler)
import Snap.Core

-- | Return a JSON response.
writeJSON :: (ToJSON a) => a -> Handler b c ()
writeJSON vals = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode vals
