module Parser (parseCommand, toNumber) where

import ApplicativeParsec
-- import Text.ParserCombinators.Parsec

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
    | DeleteID Int
    | Move (Either Int (Int, Int)) Int
    | MoveID Int Int
    | Playlist
    | PlaylistFind -- todo
    | PlaylistID (Maybe Int)
    | PlaylistInfo (Maybe (Either Int (Int, Int)))
    | PlaylistSearch -- todo
    | PlChanges Int
    | PlChangesPosID Int
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
-- the protocol specification and the 'Commands' data type
--

-- List of commands
pList = (string "command_list_begin" <|> string "command_list_ok_begin")
     *> (CList <$> many pCommand)
     <* string "command_list_end"

-- Querying MPD's status
pClearError   = ClearError  <$ string "clearerror"
pCurrentSong  = CurrentSong <$ string "currentsong"
pStatus       = Status      <$ string "status"
pStats        = Stats       <$ string "stats"
pIdle         = string "idle" *> Idle <$> optional (
                            string "database"        <|> string "update"
                        <|> string "stored_playlist" <|> string "playlist"
                        <|> string "player"          <|> string "mixer"
                        <|> string "output"          <|> string "options")

-- Playback options, TODO parse keywords aswell...
pConsume   = Consume   <* (string "consume" *> pBool)
pCrossfade = Crossfade <$> pNumber
pRandom    = Random    <$> pBool
pRepeat    = Repeat    <$> pBool
pSetvol    = Setvol    <$> pNumber
pSingle    = Single    <$> pBool

-- Controlling playback TODO keywords here too
pNext     = Next     <$  string "next"
pPause    = Pause    <$> string "pause" <*> pBool
pPlay     = Play     <$> optional pNumber
pPlayId   = PlayId   <$> optional pNumber
pPrevious = Previous <$  string "previous"

pClose = Close <$ string "close" 


--
-- Helper functions below
--

pBool = digit >>= return . (== '1')
pNumber = many1 digit >>= return . toNumber

toNumber :: [Char] -> Int
toNumber xs = foldl (\a x -> a*10 + (ord x - 48)) 0 xs
