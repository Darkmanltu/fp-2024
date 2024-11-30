module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = ["cmd", "main", "Buy", "Sell", "BuyBundle", "ViewInventory", "ResetToStarterKit", "starterKit", "advancedKit", "proKit"]