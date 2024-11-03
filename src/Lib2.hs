{-# LANGUAGE InstanceSigs #-}
module Lib2
     ( Query(..),
      parseQuery,
      State(..),
      emptyState,
      stateTransition,
      parseChar,
      parseWhitespaces,
      parseNumber,
      parseWord,
      Item(..),
      Bundle, 
      ItemName(..),
      ItemPrice(..),
      CurrencyType(..),
      parseItem,
      parseName,
      parsePrice,
      parseCurrencyAmount,
      parseCurrency,
      parseBundle,
     
    ) where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

data Query = Buy Item
           | Sell Item
           | BuyBundle Bundle
           | ViewInventory
    deriving (Show, Eq)

data Item = Item{
    itemName :: String,
    itemPrice :: ItemPrice
} 
    deriving (Show, Eq)

data ItemName = Sword | Shield | Potion | Armor | Rune
    deriving (Show, Eq)
    
data ItemPrice = SinglePrice Int CurrencyType 
               | MultiPrice [(Int, CurrencyType)]
    deriving (Show, Eq)

data CurrencyType = Gold | Silver | Copper
    deriving (Show, Eq)

type Bundle = [Item]

-- Parses the main query command (Buy or Sell and BuyBundle)
parseQuery :: String -> Either String Query
parseQuery input =
  case parseWhitespaces input of
    Right (_, rest) ->
      case parseWord rest of
        Right ("BuyBundle", rest1) ->
          case parseBundle rest1 of
            Right (bundle, rest2) -> Right (BuyBundle bundle) -- Handle BuyBundle directly
            Left err -> Left $ "Failed to parse bundle: " ++ err
        Right ("Buy", rest1) ->
          case parseItem rest1 of
            Right (item, rest2) -> Right (Buy item)
            Left err -> Left $ "Failed to parse Buy command: " ++ err
        Right ("Sell", rest1) ->
          case parseItem rest1 of
            Right (item, rest2) -> Right (Sell item)
            Left err -> Left $ "Failed to parse Sell command: " ++ err
        Right ("ViewInventory", rest1) -> Right ViewInventory
        Right (unknownCommand, _) -> Left $ "Unknown command: " ++ unknownCommand
        Left err -> Left $ "Failed to parse command: " ++ err
    Left err -> Left $ "Failed to parse query: " ++ err

-- Parses words in the input
-- used in other parsers
parseWord :: Parser String
parseWord input = 
  case parseWhitespaces input of
    Right (_, rest) ->
      let word = L.takeWhile C.isAlpha rest
          restWord = L.dropWhile C.isAlpha rest
      in if not (null word)
         then Right (word, restWord)
         else Left "Invalid character: expected a word"
    Left err -> Left err

-- Parses a specific character
parseChar :: Char -> Parser Char
parseChar c [] = Left "Unexpected end of input"
parseChar c (x:xs) | c == x    = Right (c, xs)
                   | otherwise = Left $ "Expected " ++ [c] ++ " but got " ++ [x]

-- Parses numeric values
-- <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber input =
  case parseWhitespaces input of
    Right (_, rest) -> 
      let digits = L.takeWhile C.isDigit rest
          restDigits = L.dropWhile C.isDigit rest
      in if not (null digits)
         then Right (read digits, restDigits)
         else Left "Invalid character: expected a digit"
    Left err -> Left err

-- Parses whitespace characters
parseWhitespaces :: Parser String
parseWhitespaces [] = Right ("", [])
parseWhitespaces s@(h : t) = if C.isSpace h then Right (" ", t) else Right ("", s)

-- Parses an item, e.g., "Sword 10 gold coins"

-- Parses an Item as ItemName followed by ItemPrice
-- <item> :: <item-name> <price>
parseItem :: Parser Item
parseItem input = 
  case parseName input of
    Right (itemName, rest) -> 
      case parseWhitespaces rest of
        Right (_, rest1) ->
          case parsePrice rest1 of
            Right (itemPrice, rest2) -> Right (Item itemName itemPrice, rest2)
            Left err -> Left $ "Failed to parse item price: " ++ err
        Left err -> Left $ "Failed to parse whitespace after item name: " ++ err
    Left err -> Left $ "Failed to parse item name: " ++ err

-- Parses a valid item name
-- <item_name> ::= "Sword " | "Shield " | "Potion " | "Armor " | "Rune "
parseName :: Parser String
parseName input = 
  case parseWord input of
   Right(word, rest) -> case word of
      "Sword" -> Right ("Sword", rest)
      "Shield" -> Right ("Shield", rest)
      "Potion" -> Right ("Potion", rest)
      "Armor" -> Right ("Armor", rest)
      "Rune" -> Right ("Rune", rest)
      _ -> Left "Invalid item name"
   Left err -> Left $ "Failed to parse item name: " ++ err

-- Parses an item price
-- <item_price> ::= <currency-amount> | <currency-amount> "and" <currency-amount> | <currency-amount> "and" <currency-amount> "and" <currency-amount>
parsePrice :: Parser ItemPrice
parsePrice input =
  case parseCurrencyAmount input of
    Right ((num1, currency1), rest1) ->
      case parseCurrencyAmount rest1 of
        Right ((num2, currency2), rest2) | currency2 /= currency1 ->
          case parseCurrencyAmount rest2 of
            Right ((num3, currency3), rest3) 
              | currency3 /= currency1 && currency3 /= currency2 -> 
                  Right (MultiPrice [(num1, currency1), (num2, currency2), (num3, currency3)], rest3)
            
            _ -> Right (MultiPrice [(num1, currency1), (num2, currency2)], rest2)
        _ -> Right (SinglePrice num1 currency1, rest1)
    Left err -> Left $ "Failed to parse price: " ++ err


--combining parsers
or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 a b c d = \input ->
    case a input of
        Right r1 -> Right r1  
        Left e1 ->             
            case b input of
                Right r2 -> Right r2  
                Left e2 ->             
                    case c input of
                        Right r3 -> Right r3 
                        Left e3 ->            
                            case d input of
                                Right r4 -> Right r4  
                                Left e4 -> Left (e1 ++ ", " ++ e2 ++ ", " ++ e3 ++ ", " ++ e4)  -- Combine all error messages if all fail.



and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6' comb p1 p2 p3 p4 p5 p6 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) ->
                  case p5 r4 of
                    Right (v5, r5) ->
                      case p6 r5 of
                        Right (v6, r6) -> Right (comb v1 v2 v3 v4 v5 v6, r6)
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right ((v1, v2), r2)
                Left e2 -> Left e2
        Left e1 -> Left e1
--
--
and4' :: (a -> b -> c -> d -> e ) 
      -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e 
and4' comb p1 p2 p3 p4  = \input -> 
  case p1 input of
    Right (v1, r1) -> 
      case p2 r1 of
        Right (v2, r2) -> 
          case p3 r2 of
            Right (v3, r3) -> 
              case p4 r3 of
                Right (v4, r4) -> Right (comb v1 v2 v3 v4 , r4)
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1


or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)


-- Parses a currency amount
-- <currency-amount> ::= <number> <currency>
parseCurrencyAmount :: Parser (Int, CurrencyType)
parseCurrencyAmount input =
  case parseNumber input of
    Right (num, rest1) ->
      case parseCurrency rest1 of
        Right (currency, rest2) -> Right ((num, currency), rest2)
        Left err -> Left $ "Failed to parse currency: " ++ err
    Left err -> Left $ "Failed to parse number: " ++ err

-- currency type parser
-- <currency_type> ::= "gold" | "silver" | "copper"
parseCurrency :: Parser CurrencyType
parseCurrency input =
  case parseWord input of
    Right ("gold", rest) -> Right (Gold, rest)
    Right ("silver", rest) -> Right (Silver, rest)
    Right ("copper", rest) -> Right (Copper, rest)
    _ -> Left "Expected a currency type (gold, silver, or copper) coins"


-- parsing paranthesis 
parsePara :: Parser Char
parsePara input = 
  case parseWhitespaces input of
    Right (_, rest) -> 
      case rest of
        ('(':xs) -> Right ('(', xs)
        (c:_)    -> Left $ "Expected '(' but got " ++ [c]
        []       -> Left "Expected '(' but got end of input"

-- parsing paranthesis but the other para
parsePara2 :: Parser Char
parsePara2 input = 
  case parseWhitespaces input of
    Right (_, rest) -> 
      case rest of
        (')':xs) -> Right (')', xs)
        (c:_)    -> Left $ "Expected ')' but got " ++ [c]
        []       -> Left "Expected ')' but got end of input"


--parsing bundles with 1 of 4 possible combinations
-- <bundle> ::= "(" <item> " and " <item> ")" | "(" <item> " and " <bundle> ")" | "(" <bundle> " and " <bundle> ")"
parseBundle :: Parser [Item]
parseBundle input = 
  case parsePara input of
    Right ('(', rest1) -> 
      case or4 parse2Item parseItemBundle parseBundleItem parseBundleBundle rest1 of
        Right (items, rest2) -> 
          case parsePara2 rest2 of
            Right (')', rest3) -> Right (items, rest3)
            Right (p, _) -> Left $ "Expected ')' but got " ++ [p]
            Left err -> Left $ "Failed to parse closing parenthesis: " ++ err
        Left err -> Left $ "Failed to parse bundle contents: " ++ err
    Right (p, _) -> Left $ "Expected '(' but got " ++ [p]
    Left err -> Left $ "Expected '(' at beginning of bundle: " ++ err

-- Parses bunlde made up of 2 items
-- <bundle> ::= "(" <item> "and" <item> ")"
parse2Item :: Parser [Item]
parse2Item input = 
  case parseItem input of
    Right (item1, rest1) ->
      case parseWord rest1 of
        Right ("and", rest2) ->
          case parseItem rest2 of
            Right (item2, rest3) -> Right ([item1, item2], rest3)
            Left err -> Left $ "Failed to parse second item: " ++ err
        Left err -> Left $ "Failed to parse 'and' keyword: " ++ err
    Left err -> Left $ "Failed to parse first item: " ++ err

-- Parses bundle that is made up of an item and a bundle
-- <bundle> ::= "(" <item> "and" <bundle> ")"
parseItemBundle :: Parser [Item]
parseItemBundle input = 
  case parseItem input of
    Right (item, rest1) ->
      case parseWord rest1 of
        Right ("and", rest2) ->
          case parseBundle rest2 of
            Right (bundle, rest3) -> Right (item : bundle, rest3)
            Left err -> Left $ "Failed to parse bundle: " ++ err
        Left err -> Left $ "Failed to parse 'and' keyword: " ++ err
    Left err -> Left $ "Failed to parse item: " ++ err

-- Parses bundle thats is made of first a bundle then an item
-- <bundle> ::= "(" <bundle> "and" <item> ")"
parseBundleItem :: Parser [Item]
parseBundleItem input = 
  case parseBundle input of
    Right (bundle, rest1) ->
      case parseWord rest1 of
        Right ("and", rest2) ->
          case parseItem rest2 of
            Right (item, rest3) -> Right (bundle ++ [item], rest3)
            Left err -> Left $ "Failed to parse item: " ++ err
        Left err -> Left $ "Failed to parse 'and' keyword: " ++ err
    Left err -> Left $ "Failed to parse bundle: " ++ err

-- Parses a bundle that is made up of two bundles 
-- <bundle> ::= "(" <bundle> "and" <bundle> ")"
parseBundleBundle :: Parser [Item]
parseBundleBundle input = 
  case parseBundle input of
    Right (bundle1, rest1) ->
      case parseWord rest1 of
        Right ("and", rest2) ->
          case parseBundle rest2 of
            Right (bundle2, rest3) -> Right (bundle1 ++ bundle2, rest3)
            Left err -> Left $ "Failed to parse second bundle: " ++ err
        Left err -> Left $ "Failed to parse 'and' keyword: " ++ err
    Left err -> Left $ "Failed to parse first bundle: " ++ err



-- An entity representing program state
data State = State {
    inventory :: [Item],
    gold :: Int,
    silver :: Int,
    copper :: Int
} deriving (Show, Eq)

-- Creates an initial program state
emptyState :: State
emptyState = State { inventory = [], gold = 100, silver = 100, copper = 100 }


-- Queery state transition function
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of  
    Buy item -> 
      let newState = st { inventory = item : inventory st }
       in Right (Just $ "Bought item: " ++ show item, newState)
      
    Sell item -> 
      let updatedInventory = removeItem item (inventory st)
       in if length updatedInventory == length (inventory st) -- Item not found if lengths match
             then Left $ "Item not found in inventory: " ++ show item
             else Right (Just $ "Sold item: " ++ show item, st { inventory = updatedInventory })
    
    BuyBundle bundle -> 
      let itemsInBundle =  bundle
          newState = st { inventory = itemsInBundle ++ inventory st }
       in Right (Just $ "Bought bundle: " ++ show bundle, newState)
    ViewInventory ->
      let inventoryList = inventory st
          message = if null inventoryList
                    then "Inventory is empty."
                    else "Current inventory:\n" ++ unlines (map show inventoryList)
       in Right (Just message, st)


-- fucntion to remove an item from the inventory after an item was sold
removeItem :: Item -> [Item] -> [Item]
removeItem item inventory =
  case L.elemIndex item inventory of
    Just index -> L.take index inventory ++ L.drop (index + 1) inventory
    Nothing -> inventory

