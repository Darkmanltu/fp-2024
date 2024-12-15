{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Web.Scotty
import Lib3 qualified
import Lib2 qualified
import Parsers qualified
import Control.Concurrent.Chan
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Concurrent (forkIO)
import Control.Monad.State.Strict as S (StateT, evalStateT, get)
import Control.Exception (try, SomeException)

type AppState = (TVar Lib2.State, Chan Lib3.StorageOp)

-- The cmd function
cmd :: String -> StateT AppState IO String
cmd str = do
    case Lib3.parseCommand str of
        Left e -> liftIO $ return ("PARSE ERROR: " ++ e)
        Right (c, "") -> do
            (st, chan) <- S.get
            tr <- liftIO $ Lib3.stateTransition st c chan
            case tr of
                Left e2 -> return ("ERROR: " ++ e2)
                Right m -> case m of
                    Just msg -> return msg
                    Nothing -> return "Command executed successfully"
        Right (_, r) -> liftIO $ return ("PARSE ERROR: string is not fully consumed - remaining: " ++ r)

main :: IO ()
main = do
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    state <- newTVarIO Lib2.emptyState
    _ <- forkIO $ Lib3.storageOpLoop chan

    scotty 4000 $ do
        post "/" $ do
            b <- body
            liftIO $ putStrLn $ concat ["\nRequest was: ", cs b]

            if cs b == ("GetState" :: String)
                then do
                    pureState <- liftIO $ readTVarIO state
                    text $ cs (Lib3.renderStatements $ Lib3.marshallState pureState)
                else do
                    -- Call the cmd function to process the command
                    result <- liftIO $ try (evalStateT (cmd (cs b)) (state, chan)) :: ActionM (Either SomeException String)

                    case result of
                        Left ex -> do
                            liftIO $ putStrLn $ "Error: " ++ show ex
                            text "Response: Error occurred while processing your request."
                        Right responseMsg -> do
                            text $ cs responseMsg
        Web.Scotty.get "/state" $ do
            pureState <- liftIO $ readTVarIO state
            let viewInventoryCmd = Lib2.ViewInventory
            let (Right (Just inventoryMsg, _)) = Lib2.stateTransition pureState viewInventoryCmd
          
            text $ cs inventoryMsg
