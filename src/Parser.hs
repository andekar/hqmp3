module Parser (parseCommand, toNumber) where

import ApplicativeParsec
import Data.Char

data Commands = 
    -- A command may be a list of commands
      CList [Commands] 
    -- Querying MPD's status
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
    | PlaylistFind -- todo
    | PlaylistId (Maybe Int)
    | PlaylistInfo (Maybe (Either Int (Int, Int)))
    | PlaylistSearch -- todo
    | PlChanges Int
    | PlChangesPosId Int
    | Shuffle (Maybe (Int, Int))
    | Swap Int Int
    | SwapID Int Int
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
    | RM String
    | Save String
    -- The music database
    | Count -- todo
    | Find -- todo
    | FindAdd -- todo
    | List -- todo
    | ListAll (Maybe FilePath)
    | ListAllInfo (Maybe FilePath)
    | LsInfo (Maybe FilePath)
    | Search --todo
    | Update (Maybe String)
    -- Stickers
    | Sticker -- todo, contain a sticker type?
    -- Connection Settings
    | Close
    | Kill
    | Password String
    | Ping
    -- Audio output
    | DisableAudio
    | EnableAudio
    | Output
    -- Reflection
    | Commands
    | NotCommands
    | TagTypes
    | URLHandlers
    deriving Show

parseCommand = parse pCommand "(unknown)"
-- TODO add all commands as in the datatype above
pCommand =  pClose
        <|> pPlay
        <|> pClearError
        <|> pCurrentSong
        <|> pStatus
        <|> pStatus
        <|> pStats
        <|> pList

--
-- Parsing of commands below, in the same order they appear in 
-- the protocol specification and the 'Commands' data type, almost :-)
--

-- List of commands
pList = (string "command_list_begin" <|> string "command_list_ok_begin")
     *> (CList <$> many pCommand)
     <* string "command_list_end"

-- Querying MPD's status
pClearError  = string "clearerror"  *> pure ClearError
pCurrentSong = string "currentsong" *> pure CurrentSong
pStatus      = string "status"      *> pure Status
pStats       = string "stats"       *> pure Stats
pIdle        = string "idle"        *> (pure Idle <$> optional (
                            string "database"        <|> string "update"
                        <|> string "stored_playlist" <|> string "playlist"
                        <|> string "player"          <|> string "mixer"
                        <|> string "output"          <|> string "options"))

-- Playback options
pConsume   = string "consume"   *> (Consume   <$> pBool)
pCrossfade = string "crossfade" *> (Crossfade <$> pNumber)
pRandom    = string "random"    *> (Random    <$> pBool)
pRepeat    = string "repeat"    *> (Repeat    <$> pBool)
pSetvol    = string "setvol"    *> (Setvol    <$> pNumber)
pSingle    = string "single"    *> (Single    <$> pBool)

-- Controlling playback 
pNext     = string "next"     *> pure Next
pPrevious = string "previous" *> pure Previous
pStop     = string "stop"     *> pure Stop
pPause    = string "pause"    *> (Pause  <$> pBool)
pPlay     = string "play"     *> (Play   <$> optional pNumber)
pPlayId   = string "playid"   *> (PlayId <$> optional pNumber)
pSeek     = string "seek"     *> (Seek   <$> pNumber <*> pNumber)
pSeekId   = string "seekid"   *> (SeekId <$> pNumber <*> pNumber)

-- The current playlist
pClear    = string "clear"    *> pure Clear
pAdd      = string "add"      *> (Add      <$> many anyChar) --parse file name?
pAddId    = string "addid"    *> (AddId    <$> many anyChar)
pDelete   = string "delete"   *> (Delete   <$> pNumber)
pDeleteId = string "deleteid" *> (DeleteId <$> pNumber)



pClose = string "close" *> pure Close


--
-- Helper functions below
--

pBool = digit >>= return . (== '1')
pNumber = many1 digit >>= return . toNumber

toNumber :: [Char] -> Int
toNumber xs = foldl (\a x -> a*10 + (ord x - 48)) 0 xs
