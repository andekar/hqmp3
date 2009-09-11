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

main :: IO ()
main = do
    -- start the network handler
    newChan >>= \chan -> forkIO $ startNetwork chan 6600
    gen    <- newStdGen
    state  <- defaultServerState gen >>= newIORef
    runReaderT server state

