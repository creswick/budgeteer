{-# LANGUAGE OverloadedStrings #-}
module Tests.Budgeteer.Storage where

import Data.ByteString
import qualified Data.Text as T
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck (Property, property, forAll, arbitrary)
import Test.QuickCheck.Monadic (monadicIO, run)

import Tests.Arbitrary ()
import Database.PostgreSQL.Simple (connectPostgreSQL)

import Budgeteer.Types
import Budgeteer.Db.Instances

storageTests :: ByteString -> T.TestTree
storageTests connStr =
  T.testGroup "StorageTests" [
    testProperty "Items round-trip" (testRoundTripItem connStr)
    ]

testRoundTripItem :: ByteString -> Item NoID -> Property
testRoundTripItem connStr anItem = monadicIO $ run $ do
  conn <- connectPostgreSQL connStr
  eNewItem <- store conn anItem
  case eNewItem of
    Left err -> return False
    Right newItem -> do
      mItem <- loadByID conn (getID $ itID newItem)
      let withID = anItem {itID = itID newItem}
      return $ case mItem of
                 Nothing   -> False
                 Just item -> withID == item
