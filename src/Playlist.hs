module Playlist () where

import Types
import Control.Concurrent.MVar
import Data.Array.IO

addSongs :: [FilePath] -> Player ()
addSongs = undefined

removeSongs :: [Int] -> Player ()
removeSongs = undefined

swapSongs :: [Int] -> Int -> Player ()
swapSongs = undefined


getSong :: Int -> Player Song
getSong = undefined

getNext :: Player Song
getNext = undefined

getPrevious :: Player Song
getPrevious = undefined


