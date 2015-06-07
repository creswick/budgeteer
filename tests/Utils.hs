{-# LANGUAGE DeriveDataTypeable #-}
module Utils
  ( createTestDatabase
  , TestException(..)
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C8
import           Data.Typeable ( Typeable )
import qualified System.Exit as E
import qualified System.Environment as E
import qualified System.Process as P

data TestException = ProcessFailedWithExitCode String Int
                   | ConnectionFailure String
                   deriving (Show, Typeable)

instance E.Exception TestException

-- | Create a test db and return the connection string.
-- Could fail with exceptions if the db can't be created.
createTestDatabase :: IO String
createTestDatabase = do
  dbName <- testDbName
  setupDatabase dbName
  return $ buildConnStr dbName

testDbName :: IO String
testDbName = do
    u <- E.getEnv "USER"
    return (u ++ "_budgeteer_sample")

buildConnStr :: String -> String
buildConnStr = ("postgresql:///" ++)

setupDatabase :: String -> IO ()
setupDatabase dbname = do
    dropEC <- P.system $ "dropdb --if-exists " ++ dbname
    case dropEC of
        E.ExitFailure v -> E.throwIO $ ProcessFailedWithExitCode "dropdb" v
        E.ExitSuccess -> return ()

    createEC <- P.system $ "createdb " ++ dbname
    case createEC of
        E.ExitFailure v -> E.throwIO $ ProcessFailedWithExitCode "createdb" v
        E.ExitSuccess -> return ()

    let env = [ ("DBM_DATABASE", buildConnStr dbname)
              , ("DBM_MIGRATION_STORE", migrationStore)
              , ("DBM_DATABASE_TYPE", dbType)
              ]
    h <- P.runProcess ".cabal-sandbox/bin/moo" ["upgrade"] Nothing (Just env) Nothing Nothing Nothing
    upgradeEC <- P.waitForProcess h
    case upgradeEC of
        E.ExitFailure v -> E.throwIO $ ProcessFailedWithExitCode "moo upgrade" v
        E.ExitSuccess -> return ()

migrationStore :: FilePath
migrationStore = "migrations"

dbType :: String
dbType = "postgresql"
