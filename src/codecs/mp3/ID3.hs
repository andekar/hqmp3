-- Comments in this file is partly taken from http://www.id3.org/id3v2.3.0
import qualified Data.ByteString as Bc
import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.Bits
import ApplicativeParsec
import Data.Maybe

data ID3 = 
      AENC String
    | APIC String -- will not work!!!
    | COMM String
    | ENCR String
    | ENQUA String
    | ETCO String
    | GEOB String
    | GRIB String
    | IPLS String
    | LINK String
    | MCDI String
    | MLLT String
    | OWNE String
    | PRIV String
    | PCNT String
    | POPM String
    | POSS String
    | RBUF String
    | RVAD String
    | RVRB String
    | SYLT String
    | SYLTC String
    -- | All text frame identifiers begin with "T". Only text frame identifiers
    -- begin with "T", with the exception of the "TXXX" frame

    -- | The 'Album/Movie/Show title' frame is intended for the title of the
    -- recording(/source of sound) which the audio in the file is taken from.
    | TALB String
    -- | The 'BPM' frame contains the number of beats per minute in the
    -- mainpart of the audio. The BPM is an integer and represented as a
    -- numerical string.
    | TBPM String
    -- | The 'Composer(s)' frame is intended for the name of the composer(s).
    -- They are seperated with the "/" character.
    | TCOM String
    | TCON String
    | TCOP String
    | TDAT String
    | TDLY String
    | TENC String
    | TEXT String
    | TFLT String
    | TIME String
    | TIT1 String
    | TIT2 String
    | TIT3 String
    | TKEY String
    | TLAN String
    | TLEN String
    | TMED String
    | TOAL String
    | TOFN String
    | TOLY String
    | TOPY String
    | TOPE String
    | TORY String
    | TOWN String
    | TPE1 String
    | TPE2 String
    | TPE3 String
    | TPE4 String
    | TPOS String
    | TPUB String
    | TRCK String
    | TRDA String
    | TRSN String
    | TRSO String
    | TSIZ String
    | TSRC String
    | TSSE String
    | TYER String
    deriving (Show, Eq, Read)
-- 
-- -- | The different available frames in id3v2.3
-- frames = foldl1 (<||>) (map string
--          [ "TALB" , "TBPM", "TCOM" , "TCON"
--          , "TCOP" , "TDAT", "TDLY" , "TENC"
--          , "TEXT" , "TFLT", "TIME" , "TIT1"
--          , "TIT2" , "TIT3", "TKEY" , "TLAN"
--          , "TLEN" , "TMED", "TOAL" , "TOFN"
--          , "TOLY" , "TOPY", "TOPE" , "TORY"
--          , "TOWN" , "TPE1", "TPE2" , "TPE3"
--          , "TPE4" , "TPOS", "TPUB" , "TRCK"
--          , "TRDA" , "TRSN", "TRSO" , "TSIZ"
--          , "TSRC" , "TSSE", "TYER" ])


-- pp = parse pTit2 "(unknown)"
-- 
-- -- | Parse until a frame is seen
-- pUF = manyTill anyToken frames
-- 
-- -- | Album/Movie/Show title
-- pTalb = string "PTALB" *> (TALB <$> pUF)
-- 
-- -- | The 'BPM' frame contains the number of beats per minute in the mainpart of
-- --  the audio. The BPM is an integer and represented as a numerical string.
-- pTbpm = string "TBPM" *> (TBPM <$> pUF)
-- 
-- -- | The 'Composer(s)' frame is intended for the name of the composer(s). They
-- --  are seperated with the "/" character. 
-- pTcom = string "TCOM" *> (TCOM <$> sepBy pUF (char '\\'))
-- 
-- -- |The 'Content type', which previously was stored as a one byte numeric value
-- -- only, is now a numeric string. You may use one or several of the types as
-- -- ID3v1.1 did or, since the category list would be impossible to maintain with
-- -- accurate and up to date categories, define your own.
-- pTcon = string "TCON" *> (TCON <$> pUF)
-- 
-- -- | The 'Copyright message' frame
-- pTcop = string "TCOP" *> (TCOP <$> pUF)
-- 
-- -- |The 'Date' frame is a numeric string in the DDMM format containing the date
-- -- for the recording. This field is always four characters long.
-- -- TODO: parse INTS instead
-- pTdat = string "TDAT" *> (TDAT <$> pUF)
-- 
-- -- | Playlist delay
-- -- TODO Integer
-- pTdly = string "TDLY" *> (TDLY <$> pUF)
-- 
-- -- | The 'Encoded by' frame
-- pTenc = string "PTENC" *> (TENC <$> pUF)
-- 
-- -- | The 'Lyricist(s)/Text writer(s)' frame is intended for the writer(s)
-- -- of the text or lyrics in the recording. They are seperated with the "/"
-- -- character
-- pText = string "TEXT" *> (TEXT <$> sepBy pUF (char '\\'))
-- 
-- -- | The 'File type' frame indicates which type of audio this tag defines.
-- -- TODO!!!
-- pTflt = string "TFLT" *> (TFLT <$> undefined)
-- 
-- -- | The 'Time' frame is a numeric string in the HHMM format containing the
-- -- time for the recording. This field is always four characters long. TODO INT
-- pTime = string "TIME" *> (TIME <$> pUF)
-- 
-- -- | Content group description
-- pTit1 = string "TIT1" *> (TIT1 <$> pUF)
-- 
-- -- | Title/Songname/Content description
-- pTit2 = string "TIT2" *> (TIT2 <$> pUF)
-- 
-- -- | Subtitle/Description refinement
-- pTit3 = string "TIT3" *> (TIT3 <$> pUF)
-- 
-- -- | The 'Initial key' frame contains the musical key in which the sound
-- -- starts. 
-- -- TODO check so it is correct
-- pTkey = string "TKEY" *> (TKEY <$> pUF)
-- 
-- -- | The 'Language(s)' frame should contain the languages of the text or lyrics
-- -- spoken or sung in the audio.
-- pTlan = string "TLAN" *> (TLAN <$> pUF)
-- 
-- -- | The 'Length' frame contains the length of the audiofile in milliseconds,
-- -- represented as a numeric string. 
-- -- TODO NUMERICAL STRING
-- pTlen = string "TLEN" *> (TLEN <$> pUF)
-- 
-- -- TODO
-- pTmed = undefined
-- 
-- -- TODO
-- pToal = undefined
-- 
-- pTofn = undefined
-- 
-- pToly = undefined
-- 
-- pTope = undefined

-- This data is NOT wanted and will probably not be seen
-- | Encoding stuff
-- pAenc = string "AENC" *> (AENC <$> manyTill anyToken frames)

data Header = Header { version :: Version
                     , flags :: B.ByteString
                     , siz :: Word32
                     , body :: B.ByteString                       
                     }
    deriving Show

-- | The different id3 versions
data Version = ID3v1 | ID3v2 | ID3v2_2 | ID3v2_4
    | Uknown
    deriving (Show, Eq)

-- | A ID3 Frame contains flags, theese are not considered in this file!!
data Frame = Frame { id      :: B.ByteString
                   , fSize   :: Int
                   , flag    :: B.ByteString
                   , content :: B.ByteString }
    deriving Eq

instance Show Frame where
    show (Frame id _ _ c) = B.unpack id ++ " \"" ++ B.unpack c ++ "\""

frameToData :: [Frame] -> [ID3]
frameToData = map (read . show)

mains = do content <- B.readFile "song.mp3"
           let (Just x) = getId3v2_2 content
           print x
           print $ frameToData x
           return () -- $ frameToData x

getId3v2_2 content = let (tags,rest)  = B.splitAt 3 content
                         (ver, rest') = B.splitAt 2 rest
                         ver'         = version' ver 
                         res = if (tags == tag && ver' == ID3v2_2) then
                               calID3 ver' rest'
                               else Nothing
                     in res
    where byteSize = getSize . (map fromIntegral)
          calID3 v rs = let (flags, rest) = B.splitAt 1 rs
                            (size, rest') = B.splitAt 4 rest
                            sizes           = byteSize $ Bc.unpack size
                            body = filt $ B.take (fromIntegral sizes) rest'
                        in Just $ id3Frames (B.take (fromIntegral sizes) rest')
                        -- B.take (fromIntegral sizes) rest'
                        -- Header v flags (fromIntegral sizes) body

-- | Get ID3v1
getID3v1 f = let l = B.length f
                 (_, con)        = B.splitAt (l - 128) f
                 (tag, con')     = B.splitAt 3 con
                 (title, rest)   = B.splitAt 30 con'
                 (artist, rest') = B.splitAt 30 rest
                 (album, rest'') = B.splitAt 30 rest'
                 (year, rest''') = B.splitAt 3 rest''
                 (comment, r)    = B.splitAt 30 rest'''
             in (tag, title, artist, album, year, comment, filter (== 0) (Bc.unpack r))

-- Filter all \NUL and dirty data
filt r = Bc.filter (flip notElem [ 0x00 -- \NUL
                                 ]) r

id3Frames :: B.ByteString -> [Frame]
id3Frames bs  | B.null bs = []
              | otherwise
    = let (id, rest)      = B.splitAt 4 bs
          (size, rest')   = B.splitAt 4 rest
          (flags, rest'') = B.splitAt 2 rest'
          sizeInt = fromIntegral . calcVal . map fromIntegral $ Bc.unpack size
          (content, bs')  = B.splitAt sizeInt rest''
      in (Frame id sizeInt flags (filt content)):id3Frames bs'
    where calcVal :: [Word32] -> Word32
          calcVal (x0:x1:x2:x3:[]) = let x0' = shiftL x0 24
                                         x1' = shiftL x1 16
                                         x2' = shiftL x2 8
                                     in x0' .|. x1' .|. x2' .|. x3

-- The first TAG indicating we have an id3 tag
tag :: B.ByteString
tag = Bc.pack [ 0x49
              , 0x44
              , 0x33]

version' r | r == v2    = ID3v2
           | r == v2_2  = ID3v2_2
           | r == v2_4  = ID3v2_4
           | otherwise = Uknown
    where v2 :: B.ByteString
          v2 = Bc.pack [ 0x02
                       , 0x00]
          -- Tag that indicates version 2.2 of id3
          v2_2 :: B.ByteString
          v2_2 = Bc.pack [ 0x03
                         , 0x00]
          -- Tag that indicates version 2.2 of id3
          v2_4 :: B.ByteString
          v2_4 = Bc.pack [ 0x04
                         , 0x00]

-- The size is calculated from 4 Word8 that is concatenated into a Word32
getSize :: [Word32] -> Word32
getSize (x0:x1:x2:x3:[])
    = let x0' = shiftL x0 21
          x1' = shiftL x1 14
          x2' = shiftL x2 7
          x3' = x3
      in x0' .|. x1' .|. x2' .|. x3'
getSize _ = error "Wrong argument to getSize"