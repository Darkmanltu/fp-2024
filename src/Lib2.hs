{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import Data.Char (isDigit)

-- | Data types representing the shop domain
data Query = Buy Item | Sell Item deriving (Eq, Show)
data Item = Item ItemName [Price] | Bundle Item Item deriving (Eq, Show)
data ItemName = Sword | Shield | Potion | Armor | Rune deriving (Eq, Show)
data Price = Price Int CurrencyType deriving (Eq, Show)
data CurrencyType = Gold | Silver | Copper deriving (Eq, Show)

-- | Type synonym for parser functions
type Parser a = String -> Either String (a, String)

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery input = case parseCommand input of
    Left err -> Left err
    Right (cmd, _) -> Right cmd

-- | Command parser: parses "buy" or "sell" commands
parseCommand :: Parser Query
parseCommand = 
    parseString "buy " *> (Buy <$> parseItem)
    `or2` parseString "sell " *> (Sell <$> parseItem)

-- | Item parser: parses single items or bundles
parseItem :: Parser Item
parseItem = 
    parseBundle `or2` parseSingleItem

-- | Single item parser
parseSingleItem :: Parser Item
parseSingleItem = do
    name <- parseItemName
    prices <- some parsePrice
    return (Item name prices)

-- | Bundle parser (recursive structure)
parseBundle :: Parser Item
parseBundle = do
    _ <- parseChar '('
    item1 <- parseItem
    _ <- parseString " and "
    item2 <- parseItem
    _ <- parseChar ')'
    return (Bundle item1 item2)

-- | Item name parser
parseItemName :: Parser ItemName
parseItemName =
    parseString "Sword " *> pure Sword
    `or2` parseString "Shield " *> pure Shield
    `or2` parseString "Potion " *> pure Potion
    `or2` parseString "Armor " *> pure Armor
    `or2` parseString "Rune " *> pure Rune

-- | Price parser (e.g., "6 gold coins")
parsePrice :: Parser Price
parsePrice = do
    amount <- parseNumber
    currency <- parseCurrencyType
    return (Price amount currency)

-- | Currency type parser
parseCurrencyType :: Parser CurrencyType
parseCurrencyType =
    parseString " gold coins " *> pure Gold
    `or2` parseString " silver coins " *> pure Silver
    `or2` parseString " copper coins " *> pure Copper

-- | State and state transition functions (not implemented yet)
data State = State deriving Show

emptyState :: State
emptyState = State

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state _ = Right (Just "State updated", state)

-- Helper parser combinators

-- | Parses a specific character
parseChar :: Char -> Parser Char
parseChar _ [] = Left "Empty input"
parseChar c (x:xs) = if c == x then Right (c, xs) else Left ("Expected " ++ [c] ++ " but got " ++ [x])

-- | Parses a specific string
parseString :: String -> Parser String
parseString [] input = Right ([], input)
parseString (x:xs) input = do
    (c, rest) <- parseChar x input
    (cs, rest') <- parseString xs rest
    return (c:cs, rest')

-- | Parses a number
parseNumber :: Parser Int
parseNumber input = 
    let (digits, rest) = span isDigit input
    in if null digits
       then Left "No number found"
       else Right (read digits, rest)

-- | Or combinator: tries the first parser, if it fails, tries the second
or2 :: Parser a -> Parser a -> Parser a
or2 p1 p2 input = case p1 input of
    Right res -> Right res
    Left _ -> p2 input

-- | Applies a parser multiple times (at least once)
some :: Parser a -> Parser [a]
some p = do
    x <- p
    xs <- many p
    return (x:xs)

-- | Applies a parser zero or more times
many :: Parser a -> Parser [a]
many p = some p `or2` return []

