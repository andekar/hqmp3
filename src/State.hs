module State where

import Prelude hiding (repeat)
import Types
import Database
import Control.Monad.State
import System.FilePath
import System.Directory
import qualified Data.Set as Set
import qualified Data.Map as Map

modifyStatus :: (Eq a, Eq b) => (Status -> Status) -> Server a b ()
modifyStatus f = modify $ \s -> s { status = f (status s) }

--
-- Exported stuff below
--

-- Setting various status variables

modifyVolume :: (Eq a, Eq b) => (Volume -> Volume) -> Server a b ()
modifyVolume f = modifyStatus $ \s -> s { volume = f (volume s) }

setRepeat, setSingle, setConsume :: (Eq a, Eq b) => Bool -> Server a b ()
setRepeat b  = modifyStatus $ \s -> s { repeat = b }
setSingle b  = modifyStatus $ \s -> s { single = b }
setConsume b = modifyStatus $ \s -> s { consume = b }

setPlayState :: (Eq a, Eq b) => PlayState -> Server a b ()
setPlayState ps = modifyStatus $ \s -> s { state = ps }

setXFade :: (Eq a, Eq b) => Int -> Server a b ()
setXFade i = modifyStatus $ \s -> s { xfade = i }

-- Database stuff

updateDB :: (Eq a, Eq b) => Server a b ()
updateDB = do
    db <- gets baseDir >>= lift . addDir
    modify $ \s -> s { database = db }

-- Authentication

isAuthorized :: (Eq a, Eq b) => Username -> Password -> Server a b Bool
isAuthorized user pass = do
    u <- gets username
    p <- gets password
    return $ user == u && pass == p

-- Playlist/Queue stuff

addSong :: a -> Server a b ()
addSong song = undefined

removeSong :: a -> Server a b ()
removeSong song = undefined

moveSong :: Int -> Int -> Server a b ()
moveSong from to = undefined

queueSong :: b -> Server a b ()
queueSong song = undefined

removeQueued :: b -> Server a b ()
removeQueued song = undefined

moveQueued :: Int -> Int -> Server a b ()
moveQueued from to = undefined

-- what if there is a song queued?
-- what if there are no songs in the playlist?
nextSong :: Server a b a
nextSong = undefined

prevSong :: Server a b a
prevSong = undefined
