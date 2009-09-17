module Network.ConnHandler (startNetwork) where

import Types (Command (..), Reply (..), Ok (..) )
import Network.Parser (parseCommand)

import Network (listenOn, accept, PortID (PortNumber), sClose, Socket)
import System.IO (hPutStrLn, hFlush, hClose, Handle, hGetLine)
import Control.Monad ((>>=), forever, forM_)
import Control.Monad.Reader (runReaderT, ask, asks, lift, ReaderT)
import Control.Exception (bracket)
import Control.Concurrent (forkIO, Chan, writeChan, readChan, newChan)

-- Convenient stuff
type ClientHandler a = ReaderT Client IO a
data Client = Client 
        {   chan      :: Chan (Command, Chan Reply)
          , replyChan :: Chan Reply
          , h         :: Handle
        }

-- Starts the network, or exits if there was a problem
startNetwork :: Chan (Command, Chan Reply) -> Int -> IO ()
startNetwork chan port = bracket (listenOn $ PortNumber $ fromIntegral port)
                         sClose (handler chan)

-- Server listens for connections
handler :: Chan (Command, Chan Reply) -> Socket -> IO ()
handler chan s = forever $ do
    (h,host,_) <- accept s
    print $ host ++ " connected"
    forkIO $ do 
        hPutStrLn h "OK HQmpd 0.0"
        hFlush h
        reply <- newChan
        runReaderT clientHandler (Client chan reply h)
    handler chan s


-- Main handle type for clients
clientHandler :: ClientHandler ()
clientHandler = do
    h  <- asks h
    line <- lift $ hGetLine h
    case parseCommand line of
        Left err    -> undefined
        Right Close -> do
            lift $ hPutStrLn h (show Ok)
            lift $ hClose h
        Right CListBegin -> do
            list    <- getListCommands [CListBegin]
            replies <- runCommands list False
            forM_ replies reply
        Right CListOkBegin -> do
            list    <- getListCommands [CListOkBegin]
            replies <- runCommands list True
            forM_ replies reply
        Right comm  -> runCommands [comm] False >>= mapM_ reply
  where
    reply response = do
        h <- asks h
        lift $ hPutStrLn h $ show' response
        lift $ hFlush h
        clientHandler
            
-- Read a list of commands from the network, and parse these
getListCommands :: [Command] -> ClientHandler [Command]
getListCommands cs = do
    h   <- asks h
    str <- lift $ hGetLine h
    case parseCommand str of
        Right CListEnd -> return $ reverse (CListEnd : cs)
        Right comm     -> getListCommands (comm : cs)
        Left ack       -> return $ cs

-- Sends commands to the server and listens for replies
-- from the ChanHandler
runCommands :: [Command] -> Bool -> ClientHandler [Reply]
runCommands xs list = do
    env <- ask
    forM_ xs $ \x -> lift $ writeChan (chan env) (x, (replyChan env))
    lift $ listenReply (length xs) (replyChan env) list
  where
    listenReply :: Int -> Chan Reply -> Bool -> IO [Reply]
    listenReply 0 _ _ = return []
    listenReply i chan list = do
        val <- readChan chan
        case val of
            Left ack -> return [val]
            Right _  -> do
                vals <- listenReply (i-1) chan list
                if list then return $ (Right ListOk):vals
                        else return $ (Right Ok):vals
        
-- Show for either includes ugly "Right/Left"
-- This one is WAAY happier
show' :: (Show a, Show b) => Either a b -> String
show' (Left a)  = show a
show' (Right b) = show b
