module Main where

import Types
import Network.ConnHandler
import ChanHandler
import Control.Concurrent
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
