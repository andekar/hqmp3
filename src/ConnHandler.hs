module ConnHandler (startNetwork) where

import Network
import Types
import Control.Concurrent
import Data.Char
import System.IO
import Control.Monad
import Data.List (isPrefixOf)
import ChanHandler
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Exception

startNetwork :: Show a => Chan (Command, Chan a) -> PortNumber -> IO ()
startNetwork chan port = bracket (listenOn $ PortNumber $ fromIntegral port)
                         sClose (handler chan)

handler :: Show a => Chan (Command, Chan a) -> Socket -> IO ()
handler chan s = forever $ do
    (h,_,_) <- accept s
    forkIO $ do { hPutStrLn h "OK HQmpd 0.1";
                  hFlush h;
                  clientHandler chan h}
    handler chan s

clientHandler :: Show a => Chan (Command, Chan a) -> Handle -> IO ()
clientHandler chan h = do
    str <- hGetLine h
    case parse str of
        Left ack    -> do
            hPutStrLn h ("ACK " ++ ack)
            hFlush h
            clientHandler chan h
        Right Close -> hClose h
        Right comm  -> do
            retChan <- newChan
            writeChan chan (comm,retChan)
            reply <- readChan retChan
            hPutStrLn h (show reply)
            hFlush h
            clientHandler chan h

parse :: String -> Either String Command
parse s | "close" == (head xs) = Right Close
        | "play" == (head xs)  = Right Play
        | otherwise            = Left s
    where xs = words s

