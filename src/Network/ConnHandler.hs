module Network.ConnHandler (startNetwork) where

import Types
import ChanHandler
import Network.Parser

import Network
import System.IO
import Control.Monad
import Control.Exception
import Control.Concurrent

-- Starts the network, or exits if there was a problem
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
            then getListCommands h [comm] >>= runCommands >>= reply
            else runCommands [comm] >>= reply
  where
    reply response = do
        hPutStrLn h $ show' response
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
runCommands :: [Command] -> IO (Either Ok Ack)
runCommands xs = undefined

-- Show for either includes ugly "Right/Left"
show' :: (Show a, Show b) => Either a b -> String
show' (Left a)  = show a
show' (Right b) = show b
