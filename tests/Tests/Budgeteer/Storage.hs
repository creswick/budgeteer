module Tests.Budgeteer.Storage where

import qualified Data.Text as T
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T

storageTests :: String -> T.TestTree
storageTests connStr =
  T.testGroup "StorageTests" [
    T.testCase "Items round-trip" (testRoundTripItem connStr)
    ]

testRoundTripItem :: String -> IO ()
testRoundTripItem connStr = T.assertFailure "sample test fail."
