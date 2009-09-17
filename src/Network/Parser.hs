module Network.Parser ( parseCommand ) where

import Network.ApplicativeParsec
import Types

-- Entrypoint to parsing
parseCommand = parse pCommand "(unknown)"
pCommand     =  pCListBegin     <||> pCListOkBegin  <||> pCListEnd
            -- Querying status
           <||> pClearError     <||> pCurrentSong   <||> pStatus
           <||> pStats          <||> pIdle
           -- Playback options
           <||> pConsume        <||> pCrossfade     <||> pRandom
           <||> pRepeat         <||> pSetvol        <||> pSingle
           -- Controlling playback
           <||> pNext           <||> pPrevious      <||> pStop
           <||> pPause          <||> pPlay          <||> pPlayId
           <||> pSeek           <||> pSeekId
           -- The current playlist
           <||> pClear          <||> pPlaylist      <||> pAdd
           <||> pAddId          <||> pDelete        <||> pDeleteId
           <||> pSwap           <||> pSwapId        <||> pShuffle
           <||> pMove           <||> pMoveId        <||> pPlChanges
           <||> pPlChangesPosId <||> pPlaylistId    <||> pPlaylistInfo
           <||> pPlaylistSearch <||> pPlaylistFind
           -- Stored playlists
           <||> pLoad           <||> pSave          <||> pRename
           <||> pRm             <||> pPlaylistClear <||> pPlaylistDelete
           <||> pPlaylistMove   <||> pPlaylistMove  <||> pPlaylistAdd
           <||> pListplaylists  <||> pListplaylist  <||> pListplaylistInfo
           -- The music database
           <||> pCount          <||> pFind          <||> pFindAdd
           <||> pList           <||> pSearch        <||> pListAll
           <||> pListAllInfo    <||> pLsInfo        <||> pUpdate
           -- Stickers
           <||> pSticker
           -- Connection settings
           <||> pClose          <||> pKill
           <||> pPing           <||> pPassword
           -- Audio output
           <||> pDisableOutput  <||> pEnableOutput  <||> pOutput
           -- Reflection
           <||> pCommands       <||> pNotCommands   
           <||> pTagTypes       <||> pURLHandlers

--
-- Parsing of commands below, in the same order they appear in 
-- the protocol specification and the 'Commands' data type, almost :-)
--

-- List of commands
pCListBegin   = string "command_list_begin"    *> pure CListBegin
pCListOkBegin = string "command_list_ok_begin" *> pure CListOkBegin
pCListEnd     = string "command_list_end"      *> pure CListEnd

-- Querying status
pClearError  = string "clearerror"  *> pure ClearError
pCurrentSong = string "currentsong" *> pure CurrentSong
pStatus      = string "status"      *> pure Status
pStats       = string "stats"       *> pure Stats
pIdle        = string "idle"        *> (Idle <$> optional ( 
                             string "database"        <||> string "update"
                        <||> string "stored_playlist" <||> string "playlist"
                        <||> string "player"          <||> string "mixer"
                        <||> string "output"          <||> string "options"))

-- Playback options
pConsume   = string "consume"   *> (Consume   <$> pBool)
pCrossfade = string "crossfade" *> (Crossfade <$> pNum)
pRandom    = string "random"    *> (Random    <$> pBool)
pRepeat    = string "repeat"    *> (Repeat    <$> pBool)
pSetvol    = string "setvol"    *> (Setvol    <$> pNum)
pSingle    = string "single"    *> (Single    <$> pBool)

-- Controlling playback 
pNext     = string "next"     *> pure Next
pPrevious = string "previous" *> pure Previous
pStop     = string "stop"     *> pure Stop
pPause    = string "pause"    *> (Pause  <$> pBool)
pPlay     = string "play"     *> (Play   <$> optional pNum)
pPlayId   = string "playid"   *> (PlayId <$> optional pNum)
pSeek     = string "seek"     *> (Seek   <$> pNum <*> pNum)
pSeekId   = string "seekid"   *> (SeekId <$> pNum <*> pNum)

-- The current playlist
pClear    = string "clear"    *> pure Clear
pPlaylist = string "playlist" *> pure Playlist
pAdd      = string "add"      *> (Add      <$> pString) --parse file name?
pAddId    = string "addid"    *> (AddId    <$> pString)
pDelete   = string "delete"   *> (Delete   <$> pNum)
pDeleteId = string "deleteid" *> (DeleteId <$> pNum)
pSwap     = string "swap"     *> (Swap     <$> pNum <*> pNum)
pSwapId   = string "swapid"   *> (SwapId   <$> pNum <*> pNum)
pShuffle  = string "shuffle"  *> (Shuffle  <$> optional (pTuple pNum pNum)) -- fixme with :
pMoveId   = string "moveid"   *> (MoveId   <$> pNum <*> pNum)
pMove     = string "move"     *> (Move     <$> pEither pNum (pTuple pNum pNum) --fixme with :
                                           <*> pNum)
pPlChanges      = string "plchanges"      *> (PlChanges      <$> pNum)
pPlChangesPosId = string "plchangesposid" *> (PlChangesPosId <$> pNum)
pPlaylistId     = string "playlistid"     *> (PlaylistId     <$> optional pNum)
pPlaylistInfo   = string "playlistinfo"   *> (PlaylistInfo   <$> optional 
                                             (pEither pNum (pTuple pNum pNum)))
pPlaylistSearch = string "playlistsearch" *> (PlaylistSearch <$> pTag)
pPlaylistFind   = string "playlistfind"   *> (PlaylistFind   <$> pTag)

-- Only used by playlistsearch and playlistfind, above
pTag = pArtist          <||> pAlbum      <||> pAlbumArtist
  <||> pTitle           <||> pTrack      <||> pName
  <||> pGenre           <||> pDate       <||> pComposer
  <||> pPerformer       <||> pComment    <||> pDisc

-- All those are TAGs
pArtist      = string "Artist"      *> (Artist      <$> pString)
pAlbum       = string "Album"       *> (Album       <$> pString)
pAlbumArtist = string "AlbumArtist" *> (AlbumArtist <$> pString)
pTitle       = string "Title"       *> (Title       <$> pString)
pTrack       = string "Track"       *> (Track       <$> pNum)
pName        = string "Name"        *> (Name        <$> pString)
pGenre       = string "Genre"       *> (Genre       <$> pString)
pDate        = string "Date"        *> (Date        <$> pString)
pComposer    = string "Composer"    *> (Composer    <$> pString)
pPerformer   = string "Performer"   *> (Performer   <$> pString)
pComment     = string "Comment"     *> (Comment     <$> pString)
pDisc        = string "Disc"        *> (Disc        <$> pNum)

-- Stored playlists
pLoad           = string "load"   *> (Load   <$> pString)
pSave           = string "save"   *> (Save   <$> pString)
pRename         = string "rename" *> (Rename <$> pString <*> pString)
pRm             = string "rm"     *> (Rm     <$> pString)
pPlaylistClear  = string "playlistclear"  *> (PlaylistClear  <$> pString)
pPlaylistDelete = string "playlistdelete" *> (PlaylistDelete <$> pString 
                                                             <*> pNum)
pPlaylistMove   = string "playlistmove"   *> (PlaylistMove   <$> pString
                                                             <*> pNum <*> pNum)
pPlaylistAdd    = string "playlistadd"    *> (PlaylistAdd    <$> pString 
                                                             <*> pString)
pListplaylists    = string "listplaylists"    *> pure Listplaylists
pListplaylist     = string "listplaylist"     *> (Listplaylist     <$> pString)
pListplaylistInfo = string "listplaylistinfo" *> (ListplaylistInfo <$> pString)

-- The music database
pCount       = string "count"       *> (Count       <$> pTag)
pFind        = string "find"        *> (Find        <$> (pAlbum <||> pArtist 
                                                                <||> pTitle))
pFindAdd     = string "findadd"     *> (FindAdd     <$> pTag)
pList        = string "list"        *> (List        <$> (
                                (pString *> pure ListArtist)
                           <||> (pString *> (ListAlbum <$> optional pString))))

-- The "File" constructor in Tag type is only for search
pSearch      = string "search" *> (Search <$> (pAlbum <||> pArtist <||> pAlbum 
                             <||> (string "filename" *> (File <$> pString))))

pListAll     = string "listall"     *> (ListAll     <$> optional pString)
pListAllInfo = string "listallinfo" *> (ListAllInfo <$> optional pString)
pLsInfo      = string "lsinfo"      *> (LsInfo      <$> optional pString)
pUpdate      = string "update"      *> (Update      <$> optional pString)

-- Stickers, TODO here aswell ;-)
pSticker     = string "sticker" *> (Sticker <$> pStickAction)
pStickAction = pGetSticker  <||> pSetSticker <||> pDelSticker
          <||> pListSticker <||> pFindSticker

pGetSticker = string "get" *> (StickerGet <$> pString <*> pString <*> pString)
pSetSticker = string "set" *> (StickerSet <$> pString <*> pString 
                                          <*> pString <*> pString)
pDelSticker = string "delete" *> (StickerDel <$> pString <*> pString
                                             <*> pString)
pListSticker = string "list" *> (StickerList <$> pString <*> pString)
pFindSticker = string "find" *> (StickerFind <$> pString <*> pString <*> pString)

-- Connection settings
pClose    = string "close"    *> pure Close
pKill     = string "kill"     *> pure Kill
pPing     = string "ping"     *> pure Ping
pPassword = string "password" *> (Password <$> pString)

-- Audio output
pDisableOutput = string "disableoutput" *> pure DisableOutput
pEnableOutput  = string "enableoutput"  *> pure EnableOutput
pOutput        = string "output"        *> pure Output

-- Reflection
pCommands    = string "commands"    *> pure Commands
pNotCommands = string "notcommands" *> pure NotCommands
pTagTypes    = string "tagtypes"    *> pure TagTypes
pURLHandlers = string "urlhandlers" *> pure URLHandlers

--
-- Helper functions below
--

pBool       = digit >>= return . (== '1')
pNum        = many1 digit >>= return . read
pTuple f g  = f >>= \a -> g >>= \b -> return (a,b)
pEither l r = (Left <$> l) <|> (Right <$> r)
pString     = many1 anyChar
