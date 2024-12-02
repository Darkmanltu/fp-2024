{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC
import Data.List (sort)
import Data.Ord
import Test.Tasty.QuickCheck(Arbitrary(..), elements)

import Lib2 qualified
import Lib3 qualified
import Generator qualified
import Lib2 (State(silver))



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, lib3tests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    testCase "Valid buy command parsing" $
      Lib2.parseQuery "Buy Sword 10 Gold"
        @?= Right ("", Lib2.Buy (Lib2.Item "Sword" (Lib2.SinglePrice 10 Lib2.Gold))),
    testCase "Invalid Buy command with empty input" $
       Lib2.parseQuery "" @?= Left "Failed to parse command: Invalid character: expected a word",
    testCase "Unknown command" $
      Lib2.parseQuery "Fly Sword 10 Gold"
        @?= Left "Unknown command: Fly",
        
    
    testCase "ViewInventory command parsing" $
      Lib2.parseQuery "ViewInventory"
        @?= Right ("" , Lib2.ViewInventory),
    
    testCase "Valid sell command parsing" $
      Lib2.parseQuery "Sell Potion 5 Silver"
        @?= Right ("", Lib2.Sell (Lib2.Item "Potion" (Lib2.SinglePrice 5 Lib2.Silver))),

    testCase "Invalid sell command with unknown item" $
      Lib2.parseQuery "Sell UnknownItem 5 Gold"
        @?= Left "Failed to parse Sell command: Failed to parse item: Invalid item name",

    testCase "Simple bundle parsing with two items" $
      Lib2.parseQuery "BuyBundle (Sword 5 Gold and Shield 3 Silver)"
        @?= Right ("", Lib2.BuyBundle [Lib2.Item "Sword" (Lib2.SinglePrice 5 Lib2.Gold), Lib2.Item "Shield" (Lib2.SinglePrice 3 Lib2.Silver)]),

    testCase "Nested bundle parsing with complex structure" $
      Lib2.parseQuery "BuyBundle ((Sword 5 Gold and Shield 3 Silver) and (Potion 2 Copper and Armor 4 Gold))"
        @?= Right ("", Lib2.BuyBundle [Lib2.Item "Sword" (Lib2.SinglePrice 5 Lib2.Gold), Lib2.Item "Shield" (Lib2.SinglePrice 3 Lib2.Silver), Lib2.Item "Potion" (Lib2.SinglePrice 2 Lib2.Copper), Lib2.Item "Armor" (Lib2.SinglePrice 4 Lib2.Gold)])

  ]
-- Mock items and queries from Lib2
mockItem :: Lib2.Item
mockItem = Lib2.Item "Sword" (Lib2.SinglePrice 10 Lib2.Gold)

mockQuery :: Lib2.Query
mockQuery = Lib2.Buy mockItem
instance Arbitrary Lib2.Query where 
  arbitrary = oneof 
    [
      Lib2.Buy <$> arbitrary,
      Lib2.Sell <$> arbitrary
   ]

instance Arbitrary Lib3.Statements where
    arbitrary = oneof
        [ Lib3.Single <$> arbitrary
        , Lib3.Batch <$> listOf arbitrary
        ]
instance Arbitrary Lib2.Item where
  arbitrary = do
    name <- elements ["Sword", "Shield", "Potion"]
    price <- arbitrary
    return $ Lib2.Item name price
instance Arbitrary Lib2.ItemName where
    arbitrary = elements [Lib2.Sword, Lib2.Shield, Lib2.Potion, Lib2.Armor]

instance Arbitrary Lib2.CurrencyType where
    arbitrary = elements [Lib2.Gold, Lib2.Silver, Lib2.Copper]
instance Arbitrary Lib2.ItemPrice where
    arbitrary = oneof [
        Lib2.SinglePrice <$> (getNonNegative <$> arbitrary) <*> arbitrary
      ]


-- Parsing Statements Tests
lib3tests :: TestTree
lib3tests = testGroup "parseStatements Tests"
  [ testCase "Valid Single Statement" $
      Lib3.parseStatements "Buy Sword 10 Gold" @?= Right (Lib3.Single mockQuery, ""),
    testCase "Valid Batch Statement" $
      Lib3.parseStatements "BEGIN\nBuy Sword 10 Gold\nEND" @?=
        Right (Lib3.Batch [mockQuery], ""),
    testCase "Multiple statements in Batch" $
      Lib3.parseStatements "BEGIN\nBuy Sword 10 Gold\nSell Shield 5 Silver\nEND" @?=
        Right (Lib3.Batch [mockQuery, Lib2.Sell (Lib2.Item "Shield" (Lib2.SinglePrice 5 Lib2.Silver))], "")
  ]
  
-- Property test: A Batch should always contain a list of queries
prop_batchContainsQueries :: Lib3.Statements -> Bool
prop_batchContainsQueries (Lib3.Batch queries) = not (null queries)  -- Ensures non-empty Batch
-- prop_batchContainsQueries (Lib3.Batch queries) = not []  -- Ensures non-empty Batch
prop_batchContainsQueries _ = True  -- For Single, this property does not apply.

-- Statements property test with trace
propertyTests :: TestTree
propertyTests = testGroup "Property Tests"
  [ 
    
 QC.testProperty "parseQuery . renderQuery == Right query" $
    \query -> 
      case Lib2.parseQuery (Lib3.renderQuery query) of
        Right ("", parsedQuery) -> parsedQuery == query -- Check both the parsed query and that no input remains
        _ -> False,

     QC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list),    
    
    QC.testProperty "parseQuery . renderQuery == Right query for complex queries" $
    \query -> let rendered = Lib3.renderQuery query
                  parsed = Lib2.parseQuery rendered
              in case parsed of
                  Right (_, parsedQuery) -> parsedQuery == query
                  _ -> False 
  ]