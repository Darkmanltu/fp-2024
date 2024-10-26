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
      parseWord
     
    ) where

import qualified Data.Char as C
import qualified Data.List as L
import Lessons.Lesson04 ()
import Text.ParserCombinators.ReadPrec (reset)

type Parser a = String -> Either String (a, String)

-- | Represents user input queries.
data Query = Buy Item
           | Sell Item
           | BuyBundle Bundle
    deriving (Show, Eq)

data Item = Item ItemName ItemPrice
    deriving (Show, Eq)

data ItemName = Sword 
              | Shield 
              | Potion 
              | Armor
              | Rune
    deriving (Show, Eq)
-- adjusting the ItemPrice data type to include multiple prices, and not having infinite recursion

data ItemPrice = SinglePrice Int CurrencyType 
               | MultiPrice [(Int, CurrencyType)]
    deriving (Show, Eq)

data CurrencyType = Gold | Silver | Copper
    deriving (Show, Eq)

data Bundle 
    = AndItems Item Item                -- Represents two items combined.
    | AndItemBundle Item Bundle         -- Represents an item and a bundle.
    | AndBundleItem Bundle Item         -- Represents a bundle and an item.
    | AndBundles Bundle Bundle          -- Represents two bundles combined.
    deriving (Show, Eq)

-- Parses the main query command (Buy or Sell)
parseQuery :: String -> Either String Query
parseQuery input =
  case parseWhitespaces input of
    Right (_, rest) ->
      case parseWord rest of
        Right ("Buy", rest1) ->
          case parseItem rest1 of
           -- Right (item, rest2) -> Right (BuyItem, item)
            Left err -> Left $ "Failed to parse Buy command: " ++ err
        Right ("Sell", rest1) ->
          case parseItem rest1 of
            Right (item, rest2) -> Right (Sell item)
            Left err -> Left $ "Failed to parse Sell command: " ++ err
        Right (unknownCommand, _) -> Left $ "Unknown command: " ++ unknownCommand
        Left err -> Left $ "Failed to parse command: " ++ err
    Left err -> Left $ "Failed to parse query: " ++ err

-- Parses words in the input
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


parseName :: Parser ItemName
parseName input = 
  case parseWord input of
   Right(word, rest) -> case word of
      "Sword" -> Right (Sword, rest)
      "Shield" -> Right (Shield, rest)
      "Potion" -> Right (Potion, rest)
      "Armor" -> Right (Armor, rest)
      "Rune" -> Right (Rune, rest)
      _ -> Left "Invalid item name"
   Left err -> Left $ "Failed to parse item name: " ++ err


-- Parses item prices in either single or multiple currency format
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
        -- Case for only one currency
        _ -> Right (SinglePrice num1 currency1, rest1)
    Left err -> Left $ "Failed to parse price: " ++ err

-- Helper parser to parse a single currency amount, like "10 gold"
parseCurrencyAmount :: Parser (Int, CurrencyType)
parseCurrencyAmount input =
  case parseNumber input of
    Right (num, rest1) ->
      case parseCurrency rest1 of
        Right (currency, rest2) -> Right ((num, currency), rest2)
        Left err -> Left $ "Failed to parse currency: " ++ err
    Left err -> Left $ "Failed to parse number: " ++ err

-- Parses a single currency (e.g., "gold")
parseCurrency :: Parser CurrencyType
parseCurrency input =
  case parseWord input of
    Right ("gold", rest) -> Right (Gold, rest)
    Right ("silver", rest) -> Right (Silver, rest)
    Right ("copper", rest) -> Right (Copper, rest)
    _ -> Left "Expected a currency type (gold, silver, or copper)"





  
-- Reads the item name to convert it to the ItemName type
readItemName :: String -> ItemName
readItemName "Sword" = Sword
readItemName "Shield" = Shield
readItemName "Potion" = Potion
readItemName "Armor" = Armor
readItemName "Rune" = Rune
readItemName _ = error "Invalid item name"

-- An entity representing program state
data State = State {
    inventory :: [(String, Int)],
    money :: Int
} deriving (Show, Eq)

-- Creates an initial program state
emptyState :: State
emptyState = State { inventory = [], money = 100 }

-- Updates state based on a query
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented"
