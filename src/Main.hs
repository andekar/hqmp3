module Main where
import ConnHandler
import Types
import ChanHandler
import Player
import Control.Monad.Trans
import Control.Concurrent.STM
import Control.Concurrent
import Data.IORef
import Control.Monad.State
import System.Random

main :: IO ()
main = do
    -- start the network handler
    chan <- newChan :: IO (Chan (Command, Chan Command))
    forkIO $ startNetwork chan 6600
    gen    <- newStdGen
    state  <- defaultServerState gen
    runStateT server state
    return ()