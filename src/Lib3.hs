{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements
    ) where

import Control.Concurrent (  Chan, readChan, writeChan, newChan)
import Control.Concurrent.STM(STM, TVar, stateTVar)
import Control.Concurrent.STM (TVar, readTVar, atomically, writeTVar, readTVarIO)
import Control.Exception (IOException, try)
import Data.Either(isLeft)
import qualified Lib2

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
    op <- readChan chan
    case op of
        Save content responseChan -> do
            attempt <- try (writeFile "state.txt" content) ::  IO (Either IOException ())
            case attempt of
              Left er -> do
                putStrLn $ "Error: " ++ show er
              Right _ -> do
                putStrLn "Saved successfully"
            writeChan responseChan ()
        Load responseChan -> do
            attempt2 <- try (readFile "state.txt") :: IO (Either IOException String)
            case attempt2 of
              Left er -> do
                putStrLn $ "Error: " ++ show er
                writeChan responseChan ""
              Right content -> do
                putStrLn "Loaded successfully"
                writeChan responseChan content
    storageOpLoop chan -- Continue loop

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = 
    case Lib2.parseWhitespaces input of
        Right (cmd, rest) -> 
            if cmd == "load" then
                Right (LoadCommand, rest)
            else if cmd == "save" then
                Right (SaveCommand, rest)
            else
                case parseStatements input of
                    Right (stmts, rest) -> Right (StatementCommand stmts, rest)
                    Right (stmts, rest') -> Right (StatementCommand stmts, rest')
        Left e1 -> Left e1
-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = do
  case Lib2.parseWhitespaces input of
    Left err -> Left err
    Right (_, rest) -> do
      case Lib2.parseWord rest of
        Left err -> Left err
        Right (word, rest') -> do
          if word == "BEGIN" then
            case Lib2.parseQuery rest' of
              Left err -> Left err
              Right (rest'', query) -> do
                if rest'' == "" then
                  return (Single query, rest'')
                else do
                  case parseStatements rest'' of
                    Left err -> Left err
                    Right (Batch queries, remaining) -> return (Batch (query : queries), remaining)
          else
            case Lib2.parseQuery rest' of
              Left err -> Left err
              Right (rest'', query) -> return (Single query, rest'')

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  let queries = [Lib2.ViewInventory]
  in
    Single (head queries)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements stmts =
  case stmts of
    Single query -> renderQuery query
    Batch queries ->
      "BEGIN " ++ concatMap renderQuery queries ++ "END " 



renderQuery :: Lib2.Query -> String
renderQuery query =
  case query of
    (Lib2.Buy item) -> "Buy " ++ showItemAsQuery item
    (Lib2.Sell item) -> "Sell " ++ showItemAsQuery item
    (Lib2.BuyBundle bundle) -> "AddBundle " ++ showBundleAsQuery bundle
    (Lib2.ViewInventory) -> "ViewInventory"
    Lib2.ViewInventory -> "ViewInventory"

showItemAsQuery :: Lib2.Item -> String
showItemAsQuery (Lib2.Item name price) = name ++ " " ++ showPriceAsQuery price


showPriceAsQuery :: Lib2.ItemPrice -> String
showPriceAsQuery (Lib2.SinglePrice amount currency) = show amount ++ " " ++ show currency

showBundleAsQuery :: [Lib2.Item] -> String
showBundleAsQuery items = "(" ++ concatMap showItemAsQuery items ++ ")"


processSingleQuery :: TVar Lib2.State -> Lib2.Query -> STM (Either String (Maybe String))
processSingleQuery givenState query = do
      currentState <- readTVar givenState
      case Lib2.stateTransition currentState query of
        Right (msg, updatedState) -> do
          writeTVar givenState updatedState
          case msg of
            Just str -> return $ Right $ Just str
            Nothing -> return $ Right Nothing
        Left err -> return $ Left err


printQueryResponse :: Either String (Maybe String) -> IO ()
printQueryResponse (Left err) = putStrLn $ "Failed: " ++ err
printQueryResponse (Right Nothing) = putStrLn "Success: No message returned"
printQueryResponse (Right (Just msg)) = putStrLn $ "Success: " ++ msg

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition stateVar command ioChan = do
  case command of
    StatementCommand (Batch queries) -> do
      results <- atomically $ mapM (processSingleQuery stateVar) queries
      mapM_ printQueryResponse results
      if any isLeft results
        then return $ Left "Error processing batch of queries"
        else return $ Right $ Just "Batch of queries processed successfully"


    StatementCommand (Single query) -> do
      result <- atomically $ processSingleQuery stateVar query
      result <- atomically (processSingleQuery stateVar query)
      _ <- printQueryResponse result
      if isLeft result
        then return $ Left "Error processing single query"
        else return $ Right Nothing


    LoadCommand -> do
      responceChan <- newChan
      writeChan ioChan (Load responceChan)
      loadedStateAsString <- readChan responceChan
      case parseStatements loadedStateAsString of
        Right (parsedState, _) ->
          case parsedState of
            Single Lib2.ViewInventory -> do
              atomically $ writeTVar stateVar Lib2.emptyState
              return $ Right Nothing
            _ -> return $ Left "Failed to load state from the file"
        Left _ ->
          return $ Left "Failed to parse the loaded state"


    SaveCommand -> do
      -- Getting state
      currentState <- readTVarIO stateVar
      let statementsAsString = renderStatements $ marshallState currentState
      -- Creating Chan () for response
      responceChan <- newChan
      -- Sending save operation to channel
      writeChan ioChan (Save statementsAsString responceChan)
      -- Waiting for response
      _ <- readChan responceChan
      return $ Right $ Just "State saved"


