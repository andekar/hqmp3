module Parser (parseCommand) where

import ApplicativeParsec
import Text.ParserCombinators.Parsec.Token
import Data.Char

data Commands = CList [Commands] 
    | Play (Maybe Int)
    | Close
    | ClearError
    | CurrentSong
    | Status
    | Stats
    | Consume Bool
    | Crossfade Int
    | Random Bool
    | Repeat Bool
    | Setvol Int
    | Singe Bool
    | Next
    | Pause (Maybe Bool)
    | PlayId (Maybe Int)
    | Previous
    | Seek Int Int
    | SeekId Int Int
    | Stop
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
    | Count -- todo
    | Find -- todo
    | FindAdd -- todo
    | List -- todo
    | ListAll (Maybe FilePath)
    | ListAllInfo (Maybe FilePath)
    | LsInfo (Maybe FilePath)
    | Search --todo
    | Update (Maybe String)
    | Sticker -- todo
    | Kill
    | Password String
    | Ping
    | DisableAudio
    | EnableAudio
    | Output
    | Commands
    | NotCommands
    | TagTypes
    | URLHandlers
    deriving Show

parseCommand inp = parse parse' "(unknown)" inp

-- parse :: CharParser Commands Commands
parse' = pCommand

pCommand = pClose
       <|> pPlay
       <|> pClearError
       <|> pCurrentSong
       <|> pStatus
       <|> pStats
       <|> pList

pClose = Close <$ string "close" 

pPlay = string "play" *> (Play <$> optional (read <$> manyTill digit
                                             (space <|> char '\n')))

pClearError = ClearError <$ string "clearerror"

pCurrentSong  = CurrentSong <$ string "currentsong"

pStatus  = Status <$ string "status"

pStats = Stats <$ string "stats"

pList = (string "command_list_begin" <|> string "command_list_ok_begin")
     *> (CList <$> many pCommand)
     <* string "command_list_end"