module Main where
import ConnHandler
import Types
import ChanHandler
import Player
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Concurrent
import Data.IORef
import Control.Monad.Reader
import System.Random

main = do
    chan <- lift $ atomically newTChan
    netPid <- lift $ forkIO $ startNetwork chan 6600
    cPid <- lift $ forkIO startChan chan
    -- remember to change 0 <----
    gen <- lift $ newStdGen
    ss <- newIORef $ defaultServerState netPid cPid 0 gen
    return $ runReaderT start ss

start :: Player ()
start = do
    chan <- lift $ atomically newTChan
    netpid <- lift $ forkIO $ startNetwork chan 6600
    startChan chan
