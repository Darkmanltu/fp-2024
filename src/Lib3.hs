--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    Statements (..),
    Command (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    renderQuery,
    parseBatch,
    processSingleQuery

    ) where

import Control.Concurrent (  Chan, readChan, writeChan, newChan)
import Control.Concurrent.STM(STM, TVar, stateTVar)
import Control.Concurrent.STM (TVar, readTVar, atomically, writeTVar, readTVarIO)
import Control.Exception (IOException, try)
import Data.Either(isLeft)
import qualified Lib2
import Data.Maybe (isNothing)

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
               SaveCommand |
               ResetToStarterKit Lib2.State
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = 
    case Lib2.parseWhitespaces input of
        Right (_, rest) -> 
          case Lib2.parseWord rest of
            Right (cmd, rest1) -> 
                if cmd == "load" then
                    Right (LoadCommand, rest1)
                else if cmd == "save" then
                    Right (SaveCommand, rest1)
                else
                    case parseStatements rest of
                        Right (stmts, remaining) -> Right (StatementCommand stmts, remaining)
                        Left err -> Left $ "Invalid statement: " ++ err               
            Left e1 -> Left e1
        Left e2 -> Left e2
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
        Right (word, rest') ->
          if word == "BEGIN" then
            parseBatch rest'
          else
            case Lib2.parseQuery rest of
              Left err -> Left err
              Right (remaining, query) -> Right (Single query, remaining)

-- for testing purposes only
-- parseQueries' :: String -> [Lib2.Query] -> Either String (Statements, String)
-- parseQueries' input acc =
  -- case Lib2.parseWord input of
  --   Right ("END", rest) -> Right (Batch (reverse acc), rest)
  --   Right (word, rest) -> 
  --     case Lib2.parseQuery input of
  --       Right (rest', query) -> parseQueries' rest' (query : acc)
  --       Left err -> Left err
  --       
  --   Left err -> Left err

parseBatch :: String -> Either String (Statements, String)
parseBatch input =
  case Lib2.parseWhitespaces input of
    Right (_, rest) -> parseQueries rest []
    Left err -> Left $ "Failed to parse batch: " ++ err
  where
    parseQueries :: String -> [Lib2.Query] -> Either String (Statements, String)
    parseQueries input1' acc =
      case Lib2.parseWhitespaces input1' of
        Left err -> Left err
        Right (_, input1) ->
          case Lib2.parseWord input1 of
            Right ("END", rest) -> Right (Batch (reverse acc), rest) -- Return collected queries on END
            _ -> case Lib2.parseQuery input1 of        
              Right (rest, query) -> parseQueries rest (query : acc) -- Collect query and continue
              Left err -> Left $ "Failed to parse batch query: " ++ err
            


-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    queries = map Lib2.Buy (Lib2.inventory state)

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN \n" ++ concatMap ((++ "\n") . renderQuery) qs ++ "\nEND"

--
renderQuery :: Lib2.Query -> String
renderQuery query =
  case query of
    (Lib2.Buy item) -> "Buy " ++ showItemAsQuery item
    (Lib2.Sell item) -> "Sell " ++ showItemAsQuery item
    (Lib2.BuyBundle bundle) -> "AddBundle " ++ showBundleAsQuery bundle
    (Lib2.ViewInventory) -> "ViewInventory"

-- let buyQuery = Lib2.Buy (Lib2.Item "Sword" (Lib2.SinglePrice 10 Lib2.Gold))
-- putStrLn $ Lib3.renderQuery buyQuery
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

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  currentState <- readTVar s
  case transitionThroughList currentState qs of
    Left e -> return $ Left e
    Right (msg, updatedState) -> do
      writeTVar s updatedState
      return $ Right msg
atomicStatements s (Single q) = do
  currentState <- readTVar s
  case Lib2.stateTransition currentState q of
    Left e -> return $ Left e
    Right (msg, updatedState) -> do
      writeTVar s updatedState
      return $ Right msg

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList initialState queries = go initialState queries Nothing
  where
    go state [] accMsg = Right (accMsg, state)
    go state (q:qs) accMsg =
      case Lib2.stateTransition state q of
        Left err -> Left err
        Right (msg, updatedState) -> 
          let newAccMsg = case (accMsg, msg) of
                (Nothing, m) -> m
                (Just acc, Just m) -> Just (acc ++ "; " ++ m)
                (acc, Nothing) -> acc
          in go updatedState qs newAccMsg

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
      putStrLn "Processing single query" 
      result <- atomically $ processSingleQuery stateVar query
      _ <- printQueryResponse result
      if isLeft result
        then return $ Left "Error processing single query"
          else
            case result of 
              Right (Just msg) -> return $ Right (Just msg)
              Right Nothing -> return $ Right Nothing
              Left errMsg -> return $ Left errMsg

    LoadCommand -> do
      -- Create a new channel for receiving the loaded data
      chan <- newChan :: IO (Chan String)
      
      -- Send a Load operation request
      writeChan ioChan (Load chan)
      
      -- Read the data from the channel
      qs <- readChan chan
      let trimmedQs = dropWhile (`elem` " \t\n") qs -- Trim whitespace
      
      if null trimmedQs
        then return $ Left "No state file found or file is empty"
        else case parseStatements trimmedQs of
          Left e -> 
            return $ Left $ "Failed to load state from file:\n" ++ e
          Right (qs', _) -> do
            -- Atomically update the state
            result <- atomically $ atomicStatements stateVar qs'
            case result of
              Left err -> return $ Left $ "Failed to update state:\n" ++ err
              Right msg -> return $ Right msg



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

    ResetToStarterKit kit -> do
      atomically $ writeTVar stateVar kit
      return $ Right $ Just "State reset to starter kit"

