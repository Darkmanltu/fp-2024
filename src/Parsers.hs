{-# LANGUAGE ImportQualifiedPost #-}
module Parsers
    ( 
    parse,
    parseCommands,
    parseStatements,
    parseQuery,
    parseWord,
    parseWhitespaces
    ) where


import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT, catchE)
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Class(lift)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import qualified Data.Char as C
import qualified Data.List as L
import Lib2 qualified hiding (parseWord)
import Lib3 qualified
import Lib2 ()
import Control.Monad.IO.Class (liftIO)


-- type Parser a = String -> Either String (a, String)
type Parser a = ExceptT String (State String) a

parse :: Parser a -> String -> (Either String a, String)
parse parser = runState (runExceptT parser)


or2 :: Parser a -> Parser a -> Parser a
or2 a b = do
    resultA <- a `catchE` \e1 -> do
        resultB <- b
        return resultB `catchE` \_ -> throwE e1
    return resultA


many :: Parser a -> Parser [a]
many p = (do
    x <- p
    xs <- many p
    return (x : xs)
  ) `catchE` \_ -> return []



-- | Parses user's input.
parseCommands :: Parser Lib3.Command
parseCommands = do
    --_ <- parseWhitespaces
    cmd <- lift get
    
    case parse parseWord cmd of
        (Right word, r1) ->
            if word == "load" then
                lift $ put r1 >> return Lib3.LoadCommand
            else if word == "save" then
                lift $ put r1 >> return Lib3.SaveCommand
            else 
                (case parse parseStatements cmd of
                    (Right statements, r2) -> lift $ put r2 >> return (Lib3.StatementCommand statements)
                    (Left e2, _) -> throwE e2)
        (Left e1, _) -> throwE e1        

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: Parser Lib3.Statements
parseStatements = do
    _ <- parseWhitespaces
    input <- lift get
    case parse parseWordSpace input of 
        (Right word, r1) ->
            if word == "BEGIN" then
                lift (put r1) >> parseBatch
            else
                (case parse parseQuery input of
                    (Right query, r2) -> lift $ put r2 >> return (Lib3.Single query)
                    (Left e2, _) -> throwE e2)
        (Left e1, _) -> throwE e1

        


parseBatch :: Parser Lib3.Statements
parseBatch = do
    _ <- parseWhitespaces
    queries <- many parseQuery
    _ <- parseWordSpace >>= \word -> if word == "END" then return () else throwE "Expected END"
    return $ Lib3.Batch queries

-- Helper function to run the parser
runParser :: Parser a -> String -> (Either String a, String)
runParser parser input = runState (runExceptT parser) input
-- Parses the main query command (Buy or Sell and BuyBundle)

parseQuery :: Parser Lib2.Query
parseQuery = do
    _ <- parseWhitespaces
    cmd <- parseWord
    case cmd of
        "Buy" -> do
            item <- parseItem
            return $ Lib2.Buy item
        "Sell" -> do
            item <- parseItem
            return $ Lib2.Sell item
        "BuyBundle" -> do
            bundle <- parseBundle
            return $ Lib2.BuyBundle bundle
        "ViewInventory" -> return Lib2.ViewInventory
        "ResetToStarterKit" -> do
            kit <- parseWord
            case kit of
                "starterKit" -> return $ Lib2.ResetToStarterKit Lib2.starterKit
                "advancedkit" -> return $ Lib2.ResetToStarterKit Lib2.advancedkit
                "proKit" -> return $ Lib2.ResetToStarterKit Lib2.proKit
                _ -> throwE $ "Unknown kit: " ++ kit
        _ -> throwE $ "Unknown command: " ++ cmd




or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 a b c d= do
    resultA <- a
    return resultA `catchE` \e1 -> do
        resultB <- b
        return resultB `catchE` \e2 -> do
            resultC <- c
            return resultC `catchE` \e3 -> do
                resultD <- d
                return resultD `catchE` \e4 ->  throwE (e1 ++ ", " ++ e2 ++ ", " ++ e3 ++ ", " ++ e4)


parseChar :: Char -> Parser Char
parseChar c = do
    input <- lift get
    case input of
        [] -> throwE ("Cannot find " ++ [c] ++ " in an empty input")
        s@(h:t) -> if c == h then lift $ put t >> return h else throwE (c : " is not found in " ++ s)


and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = do
    resultA <- a
    resultB <- b
    return (c resultA resultB)

parseWordSpace :: Parser String
parseWordSpace = do
    word <- parseWord
    _ <- parseWhitespaces `catchE` \_ -> return "" -- Allow optional whitespace
    return word

parseWord :: Parser String
parseWord = do
     input <- lift get
     
     case input of
        [] -> throwE "Cannot find any text made out of letters or digits in an empty input"
        str ->
            let
                lettersDigits = L.takeWhile (\c -> C.isLetter c || C.isDigit c) str
                rest = drop (length lettersDigits) str
            in
                case lettersDigits of
                    [] -> throwE "Not a letter or a digit"
                    _ -> lift $ put rest >> return lettersDigits



parseWhitespace :: Parser Char
parseWhitespace = do
    input <- lift get
    case input of
        [] -> throwE "Cannot find any whitespace in an empty input"
        s@(h:t) -> if ' ' == h then lift $ put t >> return ' ' else throwE (s ++ " does not start with a whitespace")

parseWhitespaces :: Parser String
parseWhitespaces = many parseWhitespace

parseNumber :: Parser Int
parseNumber = do
    input <- lift get
    case input of
        [] -> throwE "Cannot parse a number in an empty input"
        str ->
            let
                digits = L.takeWhile C.isDigit str
                rest = drop (length digits) str
            in
                case digits of
                    [] -> throwE "Not a number"
                    _ -> lift $ put rest >> return (read digits)


-- <item> :: <item-name> <price>
parseItem :: Parser Lib2.Item
parseItem = do
    _ <- parseWhitespaces
    itemName <- parseWord
    _ <- parseWhitespaces
    itemPrice <- parsePrice
    return $ Lib2.Item itemName itemPrice

-- Parses a valid item name
-- <item_name> ::= "Sword " | "Shield " | "Potion " | "Armor " | "Rune "
parseName :: Parser String
parseName = do
    word <- parseWord
    case word of
        "Sword" -> return "Sword"
        "Shield" -> return "Shield"
        "Potion" -> return "Potion"
        "Armor" -> return "Armor"
        "Rune" -> return "Rune"
        _ -> throwE "Invalid item name"

-- Parses an item price
-- <item_price> ::= <currency-amount> | <currency-amount> <currency-amount> | <currency-amount> <currency-amount> <currency-amount>
parsePrice :: Parser Lib2.ItemPrice
parsePrice = do
    num <- parseNumber
    _ <- parseWhitespaces
    currency <- parseCurrency
    return $ Lib2.SinglePrice num currency

-- Parses a currency amount
-- <currency-amount> ::= <number> <currency>
parseCurrencyAmount :: Parser (Int, Lib2.CurrencyType)
parseCurrencyAmount = do
    num <- parseNumber
    currency <- parseCurrency
    return (num, currency)

-- currency type parser
-- <currency_type> ::= "gold" | "silver" | "copper"
parseCurrency :: Parser Lib2.CurrencyType
parseCurrency = do
    word <- parseWord
    case word of
        "Gold" -> return Lib2.Gold
        "Silver" -> return Lib2.Silver
        "Copper" -> return Lib2.Copper
        _ -> throwE "Expected a currency type (gold, silver, or copper)"

-- parsing paranthesis 
parsePara :: Parser Char
parsePara = do
    _ <- parseWhitespaces
    input <- lift get
    case input of
        ('(':xs) -> lift (put xs) >> return '('
        (c:_)    -> throwE $ "Expected '(' but got " ++ [c]
        []       -> throwE "Expected '(' but got end of input"

-- parsing paranthesis but the other paranthesis
parsePara2 :: Parser Char
parsePara2 = do
    _ <- parseWhitespaces
    input <- lift get
    case input of
        (')':xs) -> lift (put xs) >> return ')'
        (c:_)    -> throwE $ "Expected ')' but got " ++ [c]
        []       -> throwE "Expected ')' but got end of input"

--parsing bundles with 1 of 4 possible combinations
-- <bundle> ::= "(" <item> " and " <item> ")" | "(" <item> " and " <bundle> ")" | "(" <bundle> " and " <bundle> ")"
parseBundle :: Parser [Lib2.Item]
parseBundle = do
    _ <- parseWhitespaces
    _ <- parsePara
    items <- or4 parse2Item parseItemBundle parseBundleItem parseBundleBundle
    _ <- parsePara2
    return items

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 d a b c = do
    resultA <- a
    resultB <- b
    resultC <- c
    return (d resultA resultB resultC)

-- Parses bundle made up of 2 items
-- <bundle> ::= "(" <item> "and" <item> ")"
parse2Item :: Parser [Lib2.Item]
parse2Item = do
    (item1, _, item2) <- and3 (\item1 _ item2 -> (item1, "and", item2)) parseItem parseWord parseItem
    return [item1, item2]

-- Parses bundle that is made up of an item and a bundle
-- <bundle> ::= "(" <item> "and" <bundle> ")"
parseItemBundle :: Parser [Lib2.Item]
parseItemBundle = do
    (item1, _, bundle) <- and3 (\item1 _ bundle -> (item1, "and", bundle)) parseItem parseWord parseBundle
    return (item1 : bundle)

-- Parses bundle that's made of first a bundle then an item
-- <bundle> ::= "(" <bundle> "and" <item> ")"
parseBundleItem :: Parser [Lib2.Item]
parseBundleItem = do
    (bundle, _, item) <- and3 (\bundle _ item -> (bundle, "and", item)) parseBundle parseWord parseItem
    return (bundle ++ [item])

-- Parses a bundle that is made up of two bundles
-- <bundle> ::= "(" <bundle> "and" <bundle> ")"
parseBundleBundle :: Parser [Lib2.Item]
parseBundleBundle = do
    (bundle1, _, bundle2) <- and3 (\bundle1 _ bundle2 -> (bundle1, "and", bundle2)) parseBundle parseWord parseBundle
    return (bundle1 ++ bundle2)


