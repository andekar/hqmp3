module ConnHandler (startNetwork) where

import Network
import Types
import Control.Concurrent
import Data.Char
import System.IO
import Control.Monad
import ChanHandler
import Parser
import Control.Monad.Trans
import Control.Exception
import Data.Maybe

startNetwork :: Show a => Chan (Command, Chan a) -> PortNumber -> IO ()
startNetwork chan port = bracket (listenOn $ PortNumber $ fromIntegral port)
                         sClose (handler chan)

-- Server listens for connections
handler :: Show a => Chan (Command, Chan a) -> Socket -> IO ()
handler chan s = forever $ do
    (h,_,_) <- accept s
    forkIO $ do { hPutStrLn h "OK HQmpd 0.1";
                  hFlush h;
                  clientHandler chan h}
    handler chan s

-- Main handle type for clients
clientHandler :: Show a => Chan (Command, Chan a) -> Handle -> IO ()
clientHandler chan h = do
    line <- hGetLine h
    case parseCommand line of
        Left err    -> undefined
        Right comm  -> 
            if (comm == CListBegin || comm == CListOkBegin)
            then do
                cs    <- getListCommands h [comm]
                reply <- runCommands cs
                hPutStrLn h (show $ fromJust reply) --fixme
                hFlush h
                clientHandler chan h
            else do
                reply <- runCommands [comm]
                hPutStrLn h (show $ fromJust reply) --fixme
                hFlush h
                clientHandler chan h
            
-- Read a list of commands from the network, and parse these
getListCommands :: Handle -> [Command] -> IO [Command]
getListCommands h cs = do
    str <- hGetLine h
    case parseCommand str of
        Right CListEnd -> return $ reverse (CListEnd : cs)
        Right comm     -> getListCommands h (comm : cs)
        Left ack       -> return $ cs

-- TODO
-- Will create reply channels  
runCommands :: [Command] -> IO (Maybe Ack)
runCommands xs = undefined
