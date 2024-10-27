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
        @?= Right (Lib2.Buy (Lib2.Item Lib2.Sword (Lib2.SinglePrice 10 Lib2.Gold))),
    testCase "Invalid Buy command with empty input" $
       Lib2.parseQuery "" @?= Left "Failed to parse command: Invalid character: expected a word",
    testCase "Unknown command" $
      Lib2.parseQuery "Fly Sword 10 gold"
        @?= Left "Unknown command: Fly",
        
    testCase "Parse bundle with item and bundle" $
      Lib2.parseQuery "Buy Sword 4 gold and Shield 3 silver and Potion 1 copper"
        @?= Right (Lib2.Buy (Lib2.Bundle [Lib2.Item Lib2.Sword (Lib2.SinglePrice 4 Lib2.Gold), Lib2.Item Lib2.Shield (Lib2.SinglePrice 3 Lib2.Silver), Lib2.Item Lib2.Potion (Lib2.SinglePrice 1 Lib2.Copper)])),
    testCase "ViewInventory command parsing" $
      Lib2.parseQuery "ViewInventory"
        @?= Right Lib2.ViewInventory
    
  ]
