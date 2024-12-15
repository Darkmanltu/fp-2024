{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main (main) where
import Control.Monad.Free
import Data.ByteString
import Control.Monad.Free (liftF)
import Control.Concurrent.MVar
import GHC.IO (unsafePerformIO)
import System.IO as SIO
import Network.Wreq
import Data.String.Conversions
import Control.Lens
import  Lib2 qualified
import  Lib3 qualified
import Parsers (parse, parseCommands) 
import GHC.Conc (forkIO)
import GHC.Conc.IO (threadDelay)
import Data.IORef
-- import Parsers (parseCommand)
-- import Parsers (parseCommand)

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)


data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

data MyDomainAlgebra next = Load (() -> next)
                         | Save (() -> next)
                         | BuyBundle String (() -> next)
                         | Buy String (() -> next)
                         | Sell String (()-> next )
                         | ViewInventory (String -> next)
                         | GetState (String -> next)
                            deriving (Functor)
                         
type MyDomain = Free MyDomainAlgebra

load :: MyDomain ()
load = liftF $ Load id

save :: MyDomain ()
save = liftF $ Save id

buyBundle :: String -> MyDomain ()
buyBundle bundle = liftF $ BuyBundle bundle id

buy :: String -> MyDomain ()
buy item = liftF $ Buy item id

sell :: String -> MyDomain ()
sell item = liftF $ Sell item id

viewInventory :: MyDomain String
viewInventory = liftF $ ViewInventory id

getState :: MyDomain String
getState = liftF $ GetState id

program :: MyDomain String
program = do
    --load
    viewInventory
    buy "Sword 5 Gold"
    --sell "Sword 5 Gold"
    save
    viewInventory
    state <- getState
    return state


httpLock :: MVar ()
httpLock = unsafePerformIO $ newMVar ()

runHttpRequest :: (MyDomain a -> IO a) -> MyDomain a -> IO a
runHttpRequest httpMethod givenProgram = do
    _ <- takeMVar httpLock

    result <- httpMethod givenProgram

    putMVar httpLock ()
    return result


runHttpOne :: MyDomain a -> IO a
runHttpOne (Pure a) = return a
runHttpOne (Free step) = do
    next <- runStep step
    runHttpOne next
    where
        runStep :: MyDomainAlgebra a -> IO a
        runStep (Load next) = do
            let rawRequest = cs "load" :: ByteString
            resp <- post "http://localhost:4000" rawRequest
            putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (Save next) = do
            let rawRequest = cs "save" :: ByteString
            resp <- post "http://localhost:4000" rawRequest
            putStrLn $ cs $ resp ^. responseBody
            return $ next ()
       
        runStep (Buy item next ) = do
            let rawRequest = cs $ "Buy " ++ item :: ByteString
            _ <- post "http://localhost:4000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (BuyBundle bundle next) = do
            let rawRequest = cs $ "BuyBundle " ++ bundle :: ByteString
            _ <- post "http://localhost:4000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (Sell item next) = do
            let rawRequest = cs $ "Sell " ++ item :: ByteString
            _ <- post "http://localhost:4000" rawRequest
            -- putStrLn $ cs $ resp ^. responseBody
            return $ next ()
        runStep (ViewInventory next) = do
            let rawRequest = cs "ViewInventory" :: ByteString
            resp <- post "http://localhost:4000" rawRequest
            --putStrLn $ cs $ resp ^. responseBody
            return $ next (cs $ resp ^. responseBody)
        runStep (GetState next) = do
            resp <- get "http://localhost:4000/state"
            return $ next (cs $ resp ^. responseBody)

runTest :: MyDomain a -> IO a
runTest p = do
    v <- newIORef Lib2.emptyState
    runTest' v p
    where
        runTest' :: IORef Lib2.State -> MyDomain a -> IO a
        runTest' _ (Pure a) = return a
        runTest' v (Free step) = do
            next <- runStep v step
            runTest' v next

        runStep :: IORef Lib2.State -> MyDomainAlgebra a -> IO a
        runStep v (Load next) = do
            a <- SIO.readFile "./state.txt"
            _ <- runCommandInMemory a v
            return $ next ()
        runStep v (Buy item next) = do
            _ <- runCommandInMemory ("Buy " ++ item) v
            return $ next ()
        runStep v (BuyBundle bundle next) = do
            _ <- runCommandInMemory ("BuyBundle " ++ bundle) v
            return $ next ()
        runStep v (Sell item next) = do
            _ <- runCommandInMemory ("Sell " ++ item) v
            return $ next ()
        runStep v (ViewInventory next) = do
            result <- runCommandInMemory "ViewInventory" v
            return $ next result
        runStep v (GetState next) = do
            a <- readIORef v
            return $ next $ show a
        runStep v (Save next) = do
            a <- readIORef v
            SIO.writeFile "./state.txt" (Lib3.renderStatements (Lib3.marshallState a))
            return $ next ()


runCommandInMemory :: String -> IORef Lib2.State -> IO String
runCommandInMemory commandString state = do
    case Parsers.parse Parsers.parseCommands commandString of
        (Left e, _) -> do
            return ("Error parsing input: " ++ show e)

        (Right c, "") -> do
            case c of
                Lib3.StatementCommand (Lib3.Single query) -> do
                    extractedState <- readIORef state
                    case Lib2.stateTransition extractedState query of
                        Right (msg, updatedState) -> do
                            writeIORef state updatedState
                            case msg of
                                Just str -> return str
                                Nothing -> return "Success"
                        Left err -> do
                            return ("Error in state transition: " ++ show err)
                _ -> return "Invalid command"

        (Right _, r) -> do
            return ("Remaining input: " ++ show r)



runHttpSmart :: MyDomain a -> IO a
runHttpSmart p = do
    v <- newIORef Lib2.emptyState

    -- To load the saved state to active state in server
    let rawRequestLoad = cs "load" :: ByteString
    _ <- post "http://localhost:4000" rawRequestLoad

    let rawRequestGetState = cs "GetState" :: ByteString
    serverState <- post "http://localhost:4000" rawRequestGetState
    -- putStrLn $ "\n\nServer state before: " ++ cs (serverState ^. responseBody)

    let serverStateString = cs (serverState ^. responseBody)

    result <- runHttpSmart' v serverStateString p

    finalState <- readIORef v
    -- putStrLn $ "\n\nTest state after: " ++ show finalState

    let finalStateString = Lib3.renderStatements (Lib3.marshallState finalState)

    if serverStateString == finalStateString
        then return ()
        else do
            let rawRequst = cs finalStateString :: ByteString
            _ <- post "http://localhost:4000" rawRequst
            threadDelay 30000
            let rawRequestSave = cs "save" :: ByteString
            _ <- post "http://localhost:4000" rawRequestSave
            return ()

    -- let rawRequestGetStateFinal = cs "GetState" :: ByteString
    -- serverStateFinal <- post "http://localhost:3000" rawRequestGetStateFinal
    -- putStrLn $ "\n\nServer state after: " ++ cs (serverStateFinal ^. responseBody)


    return result
    where
        runHttpSmart' :: IORef Lib2.State -> String -> MyDomain a -> IO a
        runHttpSmart' _ _ (Pure a) = return a
        runHttpSmart' v serverStateString (Free step) = do
            next <- runStep v serverStateString step
            runHttpSmart' v serverStateString next
        runStep :: IORef Lib2.State -> String -> MyDomainAlgebra a -> IO a
        runStep v serverStateString (Load next) = do
            _ <- runCommandInMemory serverStateString v
            return $ next ()
        runStep v _ (Buy item next) = do
            _ <- runCommandInMemory ("Buy " ++ item) v
            return $ next ()
        runStep v _ (BuyBundle bundle next) = do
            _ <- runCommandInMemory ("BuyBundle " ++ bundle) v
            return $ next ()
        runStep v _ (Sell item next) = do
            _ <- runCommandInMemory ("Sell " ++ item) v
            return $ next ()
        runStep v _ (ViewInventory next) = do
            result <- runCommandInMemory "ViewInventory" v
            return $ next result
        runStep v _ (GetState next) = do
            a <- readIORef v
            return $ next $ show a
        runStep _ _ (Save next) = do
            return $ next ()




makeRequestWithLock :: ByteString -> IO ()
makeRequestWithLock requestData = do
    _ <- takeMVar httpLock
    putStrLn $ "Making request: " ++ show requestData

    -- Wait 2 seconds
    threadDelay 2000000
    _ <- post "http://localhost:4000" requestData
    putStrLn $ "Received response for: " ++ show requestData

    putMVar httpLock ()


simulateRequests :: IO ()
simulateRequests = do
    -- Multiple threads for simulating concurrent requests
    _ <- forkIO $ makeRequestWithLock (cs "Request 1" :: ByteString)
    _ <- forkIO $ makeRequestWithLock (cs "Request 2" :: ByteString)
    _ <- forkIO $ makeRequestWithLock (cs "Request 3" :: ByteString)
    -- Wait for threads to finish
    threadDelay 10000000



main :: IO ()
main = do
    -- Without httpLock
     result <- runHttpRequest runHttpOne program
     putStrLn result
     return ()

    -- With httpLock
   --  result <- runHttpRequest runHttpOne program
     --putStrLn result
     --return ()

     --result <- runTest program
     --putStrLn result
     --return ()

    -- result <- runHttpRequest runHttpSmart program
    -- putStrLn result
    -- return ()

    -- For testing/showing httpLock functionality 
    -- putStrLn "Test start"
    -- simulateRequests
    -- putStrLn "Test end"
    -- return ()
