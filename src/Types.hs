module Types where

import Data.IORef
import Control.Monad.Reader
import Control.Monad.State
import System.Posix.Types
import Control.Concurrent
import System.Random
import Data.Array.MArray
import Data.Array.IO
import Control.Concurrent.MVar
import System.Time

type Player a b c = ReaderT (IORef (ServerState b c)) IO a
type Playlist     = (IOUArray Int, [Int])
type Queue a      = [a]
type Database     = [FilePath]
type BaseDirs     = [FilePath]
type Volume       = Int

data ServerState a b = ServerState
             { playlist' :: !(Playlist a)
             , username  :: !String
             , password  :: !String
             , database  :: !Database
             , baseDirs  :: !BaseDirs 
             , status    :: !Status
             , randomGen :: !StdGen
             , queue     :: !(Queue b)
               -- pids follow
             , netPid    :: !ThreadId
             , chanPid   :: !ThreadId
             , mp3Pid    :: !(Maybe ProcessID)
             }

-- | The same things that mpd outputs when asked for 'status'
data Status = Status { volume         :: MVar Volume
                     , repeat         :: MVar Bool
                     , single         :: MVar Bool
                     , consume        :: MVar Bool
                     , playlist       :: MVar Int
                     , playlistLength :: MVar Int
                     , state          :: MVar PlayState
                     , song           :: MVar Int
                     , songId         :: MVar Int
                     , time           :: MVar (Int,Int)
                     , elapsed        :: MVar (Int, Int)
                     , bitrate        :: MVar Int
                     , xfade          :: MVar Int
                     , audio          :: MVar (Int, Int, Int)
                     , nextSong       :: MVar Int
                     , nextSongId     :: MVar Int
                     } 

data PlayState = Playing | Stopped | Paused
    deriving Show

data Command =  Close | Play | UnknownCommand | Ok
    deriving Show

data Song = Song { path     :: FilePath
                 , id3      :: Maybe ID3
                 , time'    :: Int
                 , fileName :: FilePath
                 , mTime    :: ClockTime
                 }

data ID3 = ID3 { title   :: String
               , artist  :: String
               , album   :: String
               , year    :: Int
               , comment :: String
               , track   :: Int
               , genre   :: String
               }

defaultServerState gen netPid chanPid mp3Pid = do
               status <- defaultStatus
               return $ ServerState (newArray_ (0,0)) "" "" [] [] status
                        gen netPid chanPid mp3Pid

defaultStatus = do
    vol        <- newMVar 0
    repeat     <- newMVar False
    single     <- newMVar False
    consume    <- newMVar False
    playlist   <- newMVar 0
    pllength   <- newMVar 0
    state      <- newMVar Stopped
    song       <- newMVar 0
    songId     <- newMVar 0
    time       <- newMVar (0,0)
    elapsed    <- newMVar (0,0)
    bitrate    <- newMVar 0
    xfade      <- newMVar 0
    audio      <- newMVar (0, 0, 0)
    nextSong   <- newMVar 0
    nextSongId <- newMVar 0
    return $ Status vol repeat single consume playlist pllength state
                    song songId time elapsed bitrate xfade audio nextSong
                    nextSongId
