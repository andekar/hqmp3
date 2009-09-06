module ChanHandler (startChan) where

import Types
import Control.Concurrent.STM
import Control.Concurrent
import Data.Char
import System.IO
import Control.Monad
import Control.Monad.Trans
import Player

startChan :: TChan (Command, TChan Command) -> Player ()
startChan chan = do
    (comm,rchan) <- lift $ atomically $ readTChan chan
    case comm of
        Play ->  do lift $ forkIO play
                    lift $ atomically $ writeTChan rchan Ok
        _    -> undefined