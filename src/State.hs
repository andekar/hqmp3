{-# LANGUAGE BangPatterns #-}

module State where

import Prelude hiding (repeat)
import Types
import Database
import Control.Monad.State
import System.FilePath
import System.Directory
import qualified Data.Set as Set
import qualified Data.Map as Map

modifyStatus :: (Status -> Status) -> Server a b ()
modifyStatus f = modify $ \s -> s { status = f (status s) }

--
-- Exported stuff below
--

-- Set various status variables

modifyVolume :: (Volume -> Volume) -> Server a b ()
modifyVolume f = modifyStatus $ \s -> s { volume = f (volume s) }

setRepeat, setSingle, setConsume :: Bool -> Server a b ()
setRepeat b  = modifyStatus $ \s -> s { repeat = b }
setSingle b  = modifyStatus $ \s -> s { single = b }
setConsume b = modifyStatus $ \s -> s { consume = b }

setPlayState :: PlayState -> Server a b ()
setPlayState ps = modifyStatus $ \s -> s { state = ps }

setXFade :: Int -> Server a b ()
setXFade i = modifyStatus $ \s -> s { xfade = i }

-- Database stuff

updateDB :: Server a b ()
updateDB = do
    db <- gets baseDir >>= lift . addDir
    modify $ \s -> s { database = db }


-- Playlist stuff

addSong :: a -> Server a b ()
addSong song = undefined

queueSong :: b -> Server a b ()
queueSong song = undefined
