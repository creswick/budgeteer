{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Utils.WebUtils

where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Prelude hiding (log)
import           Snap.Snaplet (Handler)
import           Snap.Core
import           Text.Read (readMaybe)

enableLogging :: Bool
enableLogging = True

logText :: forall m. MonadIO m => T.Text -> m ()
logText = log . T.unpack

logBS :: forall m. MonadIO m => ByteString -> m ()
logBS = log . C8.unpack

log :: forall m. MonadIO m => String -> m ()
log str | enableLogging = liftIO $ do
  now <- getCurrentTime
  putStrLn ("[" ++ showTime now ++ "] "++str)
        | otherwise     = return ()
  where
    showTime time = formatTime defaultTimeLocale "%F %T %z" time

-- | Return a JSON response.
writeJSON :: (ToJSON a) => a -> Handler b c ()
writeJSON vals = do
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS $ encode vals

-- | Parse out an int param, passing it to a handler.
getIntParam :: ByteString -> (Int -> Handler a b c) -> Handler a b c
getIntParam paramStr fn = do
  mTheInt <- getParam paramStr
  case mTheInt of
    Nothing    -> badRequest ("Missing '" <> paramStr <> "' parameter")
    Just theIntStr -> case readMaybe $ C8.unpack theIntStr of
      Nothing  -> badRequest ("Could not parse an int from '" <> paramStr <> "' value: '" <> theIntStr <> "'")
      Just theInt -> fn theInt

-- | Respond with a BadRequest (400), logging the message.
badRequest :: ByteString -> Handler b v a
badRequest str = do
  r <- getResponse
  let msgBS = "Bad Request: " <> str
  logBS msgBS
  writeBS msgBS
  finishWith (setResponseStatus 400 msgBS r)

-- | Respond with a server error (500), logging the error message.
serverError :: ByteString -> Handler b v a
serverError str = do
  r <- getResponse
  let msgBS = "Server Error: " <> str
  logBS msgBS
  writeBS msgBS
  finishWith (setResponseStatus 500 "Server Error" r)

-- | Respond with an 'unauthorized' (401)
unauthorized :: Handler b v a
unauthorized  = do
  r <- getResponse
  writeText "Unauthorized"
  finishWith (setResponseStatus 401 "Unauthorized" r)
