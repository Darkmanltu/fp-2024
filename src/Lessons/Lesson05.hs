{-# OPTIONS_GHC -Wno-unused-top-binds -Wname-shadowing #-}
module Lessons.Lesson05 () where

import Lessons.Lesson04(Parser, parseNumber, and2', parseChar)

import qualified Data.Char as C
import qualified Data.List as L

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right ((v1, v2), r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right ((v1, v2, v3), r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

-- Combining two parsers with an OR
or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left (e1 ++ ", " ++ e2)
                
-- Parses zero or more occurrences
many :: Parser a -> Parser [a]
many p = many' p []
    where
        many' p' acc = \input ->
            case p' input of
                Left _ -> Right (acc, input)
                Right (v, r) -> many' p' (acc ++ [v]) r
or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 a b c d = \input ->
    case a input of
        Right r1 -> Right r1  -- If the first parser succeeds, return its result.
        Left e1 ->              -- If the first parser fails:
            case b input of
                Right r2 -> Right r2  -- If the second parser succeeds, return its result.
                Left e2 ->              -- If the second parser fails:
                    case c input of
                        Right r3 -> Right r3  -- If the third parser succeeds, return its result.
                        Left e3 ->              -- If the third parser fails:
                            case d input of
                                Right r4 -> Right r4  -- If the fourth parser succeeds, return its result.
                                Left e4 -> Left (e1 ++ ", " ++ e2 ++ ", " ++ e3 ++ ", " ++ e4)  -- Combine all error messages if all fail.
parseWord' :: Parser String
parseWord' input = 
  case parseWhitespaces input of
    Right (_, rest) ->
      let word = L.takeWhile C.isAlpha rest
          restWord = L.dropWhile C.isAlpha rest
      in if not (null word)
         then case parseWord word of 
            Right (word1, restWord) -> case word1 of
                "Dingus" -> Right (word, restWord)
                _ -> Left "Invalid character: expected a word"
            Left err -> Left err
         else Left "Invalid character: expected a word"
    Left err -> Left err

parseInput' :: Parser String
parseInput' = or4 parsePara3 parseManyAs parseWord' parsePara'
-- Parser for a single character '(' or ')', returned as a String
-- Parser for parentheses that returns a String without using fmap
parsePara :: Parser String
parsePara = or2 (wrapCharParser '(') (wrapCharParser ')')
  where
    wrapCharParser :: Char -> Parser String
    wrapCharParser ch = \input ->
      case parseChar ch input of
        Right (c, rest) -> Right ([c], rest)  -- Wrap `c` in a list to return as `String`
        Left err -> Left err


parsePara' :: Parser String
parsePara' = \input ->
    case parseChar '(' input of 
        Right(c, rest) -> Right ([c], rest)
        Left err -> Left err


parsePara2 :: Parser String
parsePara2 = \input ->
    case and2 parsePara parseWord input of
        Right ((para, word), rest) -> Right (para ++ word, rest)
        Left err -> Left err
-- Main parser combining parentheses and words
parseInput :: Parser String
parseInput = or2 parsePara3 parseWord 

parsePara3 :: Parser String
parsePara3 = \input ->
    case and3 parsePara parseWord parseWord input of
        Right ((para, word, word2), rest) -> Right ( word  ++ word2, rest)
        Left err -> Left err

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

-- Parses whitespaces and returns the rest of the input
parseWhitespaces :: Parser String
parseWhitespaces [] = Right ("", [])
parseWhitespaces s@(h : t) = if C.isSpace h then Right (" ", t) else Right ("", s)

-- >>> parseManyAs ""
-- Right ("","")
-- >>> parseManyAs "aaab"
-- Right ("aaa","b")
-- >>> parseManyAs "baaab"
-- Right ("","baaab")
parseManyAs :: Parser [Char]
parseManyAs = many (parseChar 'a')

-- >>> parseListOfNumbers ""
-- Left "empty input, cannot parse a number"
-- >>> parseListOfNumbers "a"
-- Left "not a number"
-- >>> parseListOfNumbers "123"
-- Right ([123],"")
-- >>> parseListOfNumbers "123,123123,31232"
-- Right ([123,123123,31232],"")
parseListOfNumbers :: Parser [Integer]
parseListOfNumbers = and2' (:)
                        parseNumber
                        (many (and2' (\_ b -> b) (parseChar ',') parseNumber))

-- >>> parseTwoNumbers "3123"
-- Left "Cannot find , in an empty input"
-- >>> parseTwoNumbers "3123,"
-- Left "empty input, cannot parse a number"
-- >>> parseTwoNumbers "3123,4234a"
-- Right ((3123,4234),"a")
parseTwoNumbers :: Parser (Integer, Integer)
parseTwoNumbers = and2' (\a b -> (a, b)) parseNumber (and2' (\_ b -> b) (parseChar ',') parseNumber)

-- Data type examples
data Person = Person String Int

surname :: Person -> String
surname (Person s _) = s

-- Person' type with record syntax
data Person' = Person' {
    name :: String,
    age :: Int
} deriving Show

-- Example data and pure function
me :: Person'
me = Person' "Vi" 13

pureBusinessLogic :: String -> String
pureBusinessLogic n = "Hello, " ++ n

game :: IO String
game = do
    putStrLn "What is your name?"
    name' <- getLine
    return (pureBusinessLogic name')
