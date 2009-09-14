module Types where

import Data.IORef
import Control.Monad.State
import System.Posix.Types
import Control.Concurrent
import System.Random
import Data.Sequence as Seq
import Data.Set as Set
import Data.Map as Map
import Control.Concurrent.MVar
import System.Time

type CommandChan    = Chan (Command, Chan Command)
type PlayerChan a b = ( Chan (ServerState a b -> ServerState a b)
                      , Chan PlayerCommand)

type Server a b c = StateT (ServerState a b) IO c

-- Structure          Dir       .mp3s               SubDirs
data Database = Dir FilePath (Set Song) (Map FilePath Database)
    deriving Show

-- Really, what type should this be?
type Playlist a   = Seq a
type Queue a      = [a]
type Volume       = Int
type Username     = String
type Password     = String

data (Eq a, Eq b) => ServerState a b = ServerState
             { playlist   :: !(Playlist a)
             , username   :: !Username
             , password   :: !Password
             , database   :: !Database
             , baseDir    :: !FilePath
             , status     :: !Status
             , randomGen  :: !StdGen
             , queue      :: !(Queue b)
             , playerChan :: PlayerChan a b
             , commChan   :: !CommandChan
             }

-- | The same things that mpd outputs when asked for 'status'
data Status = Status 
             { volume         :: Volume
             , repeat         :: Bool
             , single         :: Bool
             , consume        :: Bool
             , playlistID     :: Int
             , playlistLength :: Int
             , state          :: PlayState
             , song           :: Int
             , songId         :: Int
             , time           :: (Int,Int)
             , elapsed        :: (Int, Int)
             , bitrate        :: Int
             , xfade          :: Int
             , audio          :: (Int, Int, Int)
             , nextSong       :: Int
             , nextSongId     :: Int
             } 

data PlayState = Playing | Stopped | Paused
    deriving Show

data Command =  Close | Play | UnknownCommand | Ok
    deriving Show

data PlayerCommand = NextSong

data Song = Song 
            { path     :: FilePath
            , id3      :: Maybe ID3
            , time'    :: Int
            , fileName :: FilePath
            , mTime    :: ClockTime
            }
    deriving Show

instance Eq Song where
    Song p _ _ _ _ == Song p' _ _ _ _ = p == p'
instance Ord Song where
    Song p _ _ _ _ <= Song p' _ _ _ _ = p <= p'

data ID3 = ID3 
        { title   :: String
        , artist  :: String
        , album   :: String
        , year    :: Int
        , comment :: String
        , track   :: Int
        , genre   :: String
        }
    deriving Show

defaultServerState :: (Eq a, Eq b) => StdGen -> IO (ServerState a b)
defaultServerState gen = do
    commChan <- newChan
    player1  <- newChan
    player2  <- newChan
    time     <- getClockTime
    return $ ServerState Seq.empty "" "" (Dir "" Set.empty Map.empty) [] 
                         defaultStatus gen [] (player1,player2) commChan

defaultStatus :: Status
defaultStatus = let
    vol         = 0
    repeat      = False
    single      = False
    consume     = False
    playlist    = 0
    pllength    = 0
    state       = Stopped
    song        = 0
    songId      = 0
    time        = (0,0)
    elapsed     = (0,0)
    bitrate     = 0
    xfade       = 0
    audio       = (0, 0, 0)
    nextSong    = 0
    nextSongID  = 0
    in Status vol repeat single consume playlist pllength state
              song songId time elapsed bitrate xfade audio nextSong 
              nextSongID 

instance Read ClockTime where
    readList x = [([toClockTime (read x :: CalendarTime)], "")]