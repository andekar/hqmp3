module ChanHandler (server) where

import Types
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader
import Player
import Data.IORef

server :: Server Song Song ()
server = do
    checkPlayerChan
    checkCommandChan
    lift $ threadDelay 100
    server

checkPlayerChan :: Server Song Song ()
checkPlayerChan = do
    return ()

checkCommandChan :: Server Song Song ()
checkCommandChan = do 
    return ()

-- Convenient function for getting values from a channel
maybeGetChan :: Chan a -> IO (Maybe a)
maybeGetChan c = isEmptyChan c >>= \e -> case e of
    True -> return Nothing
    _    -> readChan c >>= return . Just
