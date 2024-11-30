{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )


import Lib2 qualified



main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [
    testCase "Valid buy command parsing" $
      Lib2.parseQuery "Buy Sword 10 gold"
        @?= Right (Lib2.Buy (Lib2.Item "Sword" (Lib2.SinglePrice 10 Lib2.Gold))),
    testCase "Invalid Buy command with empty input" $
       Lib2.parseQuery "" @?= Left "Failed to parse command: Invalid character: expected a word",
    testCase "Unknown command" $
      Lib2.parseQuery "Fly Sword 10 gold"
        @?= Left "Unknown command: Fly",
        
    
    testCase "ViewInventory command parsing" $
      Lib2.parseQuery "ViewInventory"
        @?= Right Lib2.ViewInventory,
    
    testCase "Valid sell command parsing" $
      Lib2.parseQuery "Sell Potion 5 silver"
        @?= Right (Lib2.Sell (Lib2.Item "Potion" (Lib2.SinglePrice 5 Lib2.Silver))),

    testCase "Invalid sell command with unknown item" $
      Lib2.parseQuery "Sell UnknownItem 5 gold"
        @?= Left "Failed to parse Sell command: Failed to parse item: Invalid item name",

    testCase "Simple bundle parsing with two items" $
      Lib2.parseQuery "BuyBundle (Sword 5 gold and Shield 3 silver)"
        @?= Right (Lib2.BuyBundle [Lib2.Item "Sword" (Lib2.SinglePrice 5 Lib2.Gold), Lib2.Item "Shield" (Lib2.SinglePrice 3 Lib2.Silver)]),

    testCase "Nested bundle parsing with complex structure" $
      Lib2.parseQuery "BuyBundle ((Sword 5 gold and Shield 3 silver) and (Potion 2 copper and Armor 4 gold))"
        @?= Right (Lib2.BuyBundle [Lib2.Item "Sword" (Lib2.SinglePrice 5 Lib2.Gold), Lib2.Item "Shield" (Lib2.SinglePrice 3 Lib2.Silver), Lib2.Item "Potion" (Lib2.SinglePrice 2 Lib2.Copper), Lib2.Item "Armor" (Lib2.SinglePrice 4 Lib2.Gold)])

  ]