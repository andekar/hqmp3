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
    deriving (Show, Read)

-- Really, what type should this be?
type Playlist a   = Seq a
type Queue a      = [a]
type Volume       = Int
type Username     = String
type Password     = String

data PlayerCommand = NextSong

data (Ord a, Ord b) => ServerState a b = ServerState
             { playlist   :: !(Playlist a)
             , username   :: !Username
             , password   :: !Password
             , database   :: !Database
             , baseDir    :: !FilePath
             , status     :: !PlayerStatus
             , randomGen  :: !StdGen
             , queue      :: !(Queue b)
             , playerChan :: PlayerChan a b
             , commChan   :: !CommandChan
             }

-- | The same things that mpd outputs when asked for 'status'
data PlayerStatus = PlayerStatus 
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

data Song = Song 
            { path     :: FilePath
            , id3      :: Maybe ID3
            , time'    :: Int
            , fileName :: FilePath
            , mTime    :: ClockTime
            }
    deriving (Show, Read)

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
    deriving (Show, Read)

defaultServerState ::  StdGen -> IO (ServerState String String)
defaultServerState gen = do
    commChan <- newChan
    player1  <- newChan
    player2  <- newChan
    time     <- getClockTime
    return $ ServerState Seq.empty "" "" (Dir "" Set.empty Map.empty) []
                         defaultStatus gen [] (player1,player2) commChan

defaultStatus :: PlayerStatus
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
    in PlayerStatus vol repeat single consume playlist pllength state
              song songId time elapsed bitrate xfade audio nextSong 
              nextSongID 

instance Read ClockTime where
    readList x = [([toClockTime (read x :: CalendarTime)], "")]

--
-- Types used primarily in the parser below
--

type Reply = Either Ack Ok
data Ok  = Ok | ListOk 
instance Show Ok where
    show Ok     = "OK"
    show ListOk = "list_OK"
data Ack = Ack         deriving Show

data Command = 
    -- A command may be a list of commands
      CListBegin
    | CListOkBegin
    | CListEnd
    -- Querying status
    | ClearError
    | CurrentSong
    | Status
    | Stats
    | Idle (Maybe String) -- polling
    -- Playback options
    | Consume Bool
    | Crossfade Int
    | Random Bool
    | Repeat Bool
    | Setvol Int
    | Single Bool
    -- Controlling playback
    | Next
    | Pause Bool
    | Play (Maybe Int)
    | PlayId (Maybe Int)
    | Previous
    | Seek Int Int
    | SeekId Int Int
    | Stop
    -- The current playlist
    | Add String
    | AddId String
    | Clear
    | Delete Int
    | DeleteId Int
    | Move (Either Int (Int, Int)) Int
    | MoveId Int Int
    | Playlist
    | PlaylistFind Tag
    | PlaylistId (Maybe Int)
    | PlaylistInfo (Maybe (Either Int (Int, Int)))
    | PlaylistSearch Tag
    | PlChanges Int
    | PlChangesPosId Int
    | Shuffle (Maybe (Int, Int))
    | Swap Int Int
    | SwapId Int Int
    -- Stored playlists
    | Listplaylist String
    | ListplaylistInfo String
    | Listplaylists
    | Load String
    | PlaylistAdd String String
    | PlaylistClear String
    | PlaylistDelete String Int
    | PlaylistMove String Int Int
    | Rename String String
    | Rm String
    | Save String
    -- The music database
    | Count Tag
    | Find Tag -- not a full tag, see Parser
    | FindAdd Tag 
    | List ListTag
    | ListAll (Maybe FilePath)
    | ListAllInfo (Maybe FilePath)
    | LsInfo (Maybe FilePath)
    | Search Tag -- not a full tag, see Parser TODO
    | Update (Maybe String)
    -- Stickers
    | Sticker -- todo, contain a sticker type?
    -- Connection Settings
    | Close
    | Kill
    | Password String
    | Ping
    -- Audio output
    | DisableOutput
    | EnableOutput
    | Output
    -- Reflection
    | Commands
    | NotCommands
    | TagTypes
    | URLHandlers

data ListTag = 
    ListArtist
  | ListAlbum (Maybe String)

data Tag = 
    Artist String 
  | Album String
  | AlbumArtist String
  | Title String
  | Track Int
  | Name String
  | Genre String
  | Date String
  | Composer String
  | Performer String
  | Comment String
  | Disc Int

