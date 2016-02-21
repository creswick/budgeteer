module Main where


import Data.ByteString (ByteString)
import Test.Tasty (TestTree, defaultIngredients, defaultMainWithIngredients
                  , testGroup)
import Test.Tasty.Ingredients (Ingredient)
import Test.Tasty.Runners.AntXML (antXMLRunner)

import Utils (createTestDatabase)
import Tests.Budgeteer.Storage (storageTests)


ingredients :: [Ingredient]
ingredients = antXMLRunner : defaultIngredients

main :: IO ()
main = do
  connStr <- createTestDatabase
  defaultMainWithIngredients ingredients (tests connStr)

tests :: ByteString -> TestTree
tests connStr = testGroup "Tests"
  [ storageTests connStr
  ]
