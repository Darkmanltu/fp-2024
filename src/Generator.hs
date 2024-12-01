module Generator (
genItem,
genQuery,
genStatements,
genName,
genPrice

) where
import Test.QuickCheck as QC
import Lib2
import Lib3 

-- Generate names for items
genName :: Gen String
genName = elements ["Sword", "Shield", "Potion", "Rune"]

-- Generate prices for items
genPrice :: Gen Lib2.ItemPrice
genPrice = do
  amount <- choose (1, 100)
  currency <- elements [Lib2.Gold, Lib2.Silver, Lib2.Copper]
  return $ Lib2.SinglePrice amount currency

-- Generate individual items
genItem :: Gen Lib2.Item
genItem = do
  name <- genName
  price <- genPrice
  return $ Lib2.Item name price

-- Generate queries
genQuery :: Gen Lib2.Query
genQuery = oneof
  [ Lib2.Buy <$> genItem
  , Lib2.Sell <$> genItem
  ]

-- Generate statements
genStatements :: Gen Statements
genStatements = oneof
  [ Single <$> genQuery
  , Batch <$> listOf genQuery
  ]
