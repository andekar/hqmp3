module ChanHandler (server) where

import Types
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Monad.Trans
import Player

server :: (Ord a, Ord b) => Server a b ()
server = do
    checkPlayerChan
    checkCommandChan
    lift $ threadDelay 100
    server

checkPlayerChan :: Server a b ()
checkPlayerChan = do
    (pChan,rChan) <- gets playerChan
    comm <- lift $ maybeGetChan pChan
    case comm of
        Nothing -> return ()
        Just c  -> do
            playerCommand c
            return ()

checkCommandChan :: Server a b ()
checkCommandChan = do 
    return ()

-- Convenient function for getting values from a channel
maybeGetChan :: Chan a -> IO (Maybe a)
maybeGetChan c = isEmptyChan c >>= \e -> case e of
    True -> return Nothing
    _    -> readChan c >>= return . Just


playerCommand :: Command -> Server a b ()
playerCommand comm = undefined
