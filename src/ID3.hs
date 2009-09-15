-- Comments in this file is partly taken from http://www.id3.org/id3v2.3.0
import qualified Data.ByteString as Bc
import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.Bits
import ApplicativeParsec

data ID3v2_3 = 
      TALB String
    | TBPM String
    | TCOM [String]
    | TCON String
    | TCOP String
    | TDAT String
    | TDLY String
    | TENC String
    | TEXT [String]
    | TFLT String
    | TIME String
    | TIT1 String
    | TIT2 String
    | TIT3 String
    | TKEY String
    | TLAN String
    | TLEN String
    | TMED
    | TOAL
    | TOFN
    | TOLY
    | TOPY
    | TOPE
    | TORY
    | TOWN
    | TPE1
    | TPE2
    | TPE3
    | TPE4
    | TPOS
    | TPUB
    | TRCK
    | TRDA
    | TRSN
    | TRSO
    | TSIZ
    | TSRC
    | TSSE
    | TYER
    deriving Show

-- | The different available frames in id3v2.3
frames = foldl1 (<||>) (map string
         [ "TALB" , "TBPM"
         , "TCOM" , "TCON"
         , "TCOP" , "TDAT"
         , "TDLY" , "TENC"
         , "TEXT" , "TFLT"
         , "TIME" , "TIT1"
         , "TIT2" , "TIT3"
         , "TKEY" , "TLAN"
         , "TLEN" , "TMED"
         , "TOAL" , "TOFN"
         , "TOLY" , "TOPY"
         , "TOPE" , "TORY"
         , "TOWN" , "TPE1"
         , "TPE2" , "TPE3"
         , "TPE4" , "TPOS"
         , "TPUB" , "TRCK"
         , "TRDA" , "TRSN"
         , "TRSO" , "TSIZ"
         , "TSRC" , "TSSE"
         , "TYER" ])

mains f = do content <- B.readFile f
             return $ getID3v1 content


-- | Get ID3v1
-- getID3v1 :: B.ByteString -> 
getID3v1 f = let l = B.length f
                 (_, con)        = B.splitAt (l - 128) f
                 (tag, con')     = B.splitAt 3 con
                 (title, rest)   = B.splitAt 30 con'
                 (artist, rest') = B.splitAt 30 rest
                 (album, rest'') = B.splitAt 30 rest'
                 (year, rest''') = B.splitAt 3 rest''
                 (comment, r)    = B.splitAt 30 rest'''
             in (tag, title, artist, album, year, comment, filter (== 0) (Bc.unpack r))

pp = parse pTit2 "(unknown)"

-- | Parse until a frame is seen
pUF = manyTill anyToken frames

-- | Album/Movie/Show title
pTalb = string "PTALB" *> (TALB <$> pUF)

-- | The 'BPM' frame contains the number of beats per minute in the mainpart of
--  the audio. The BPM is an integer and represented as a numerical string.
pTbpm = string "TBPM" *> (TBPM <$> pUF)

-- | The 'Composer(s)' frame is intended for the name of the composer(s). They
--  are seperated with the "/" character. 
pTcom = string "TCOM" *> (TCOM <$> sepBy pUF (char '\\'))

-- |The 'Content type', which previously was stored as a one byte numeric value
-- only, is now a numeric string. You may use one or several of the types as
-- ID3v1.1 did or, since the category list would be impossible to maintain with
-- accurate and up to date categories, define your own.
pTcon = string "TCON" *> (TCON <$> pUF)

-- | The 'Copyright message' frame
pTcop = string "TCOP" *> (TCOP <$> pUF)

-- |The 'Date' frame is a numeric string in the DDMM format containing the date
-- for the recording. This field is always four characters long.
-- TODO: parse INTS instead
pTdat = string "TDAT" *> (TDAT <$> pUF)

-- | Playlist delay
-- TODO Integer
pTdly = string "TDLY" *> (TDLY <$> pUF)

-- | The 'Encoded by' frame
pTenc = string "PTENC" *> (TENC <$> pUF)

-- | The 'Lyricist(s)/Text writer(s)' frame is intended for the writer(s)
-- of the text or lyrics in the recording. They are seperated with the "/"
-- character
pText = string "TEXT" *> (TEXT <$> sepBy pUF (char '\\'))

-- | The 'File type' frame indicates which type of audio this tag defines.
-- TODO!!!
pTflt = string "TFLT" *> (TFLT <$> undefined)

-- | The 'Time' frame is a numeric string in the HHMM format containing the
-- time for the recording. This field is always four characters long. TODO INT
pTime = string "TIME" *> (TIME <$> pUF)

-- | Content group description
pTit1 = string "TIT1" *> (TIT1 <$> pUF)

-- | Title/Songname/Content description
pTit2 = string "TIT2" *> (TIT2 <$> pUF)

-- | Subtitle/Description refinement
pTit3 = string "TIT3" *> (TIT3 <$> pUF)

-- | The 'Initial key' frame contains the musical key in which the sound
-- starts. 
-- TODO check so it is correct
pTkey = string "TKEY" *> (TKEY <$> pUF)

-- | The 'Language(s)' frame should contain the languages of the text or lyrics
-- spoken or sung in the audio.
pTlan = string "TLAN" *> (TLAN <$> pUF)

-- | The 'Length' frame contains the length of the audiofile in milliseconds,
-- represented as a numeric string. 
-- TODO NUMERICAL STRING
pTlen = string "TLEN" *> (TLEN <$> pUF)

-- TODO
pTmed = undefined

-- TODO
pToal = undefined

pTofn = undefined

pToly = undefined

pTope = undefined

-- This data is NOT wanted and will probably not be seen
-- | Encoding stuff
-- pAenc = string "AENC" *> (AENC <$> manyTill anyToken frames)

data Header = Header { version :: B.ByteString
                     , flags :: B.ByteString
                     , siz :: Word32
                     , body :: B.ByteString                       
                     }

gets = do content <- B.readFile "song4.mp3"
          let (tags,rest) = B.splitAt 3 content
              (version, rest') = B.splitAt 2 rest
              (flags, rest'') = B.splitAt 1 rest'
              (size, rest''') = B.splitAt 4 rest''
              sizes = byteSize $ Bc.unpack size
              body = filt $ B.take (fromIntegral sizes) rest'''
          return (tags, version, flags, body)
    where byteSize = getSize . (map fromIntegral)

-- Filter all \NUL and dirty data
filt r = let zero = Bc.singleton (0x00 :: Word8)
             res = Bc.filter (flip notElem [ 0x00 -- \NUL
                                           , 0x11 -- \DC1
                                           , 0x12 -- \DC2
                                           , 0x09 -- \t
                                           ]) r
         in res

-- The first TAG indicating we have an id3 tag
tag :: [Word8]
tag = [ 0x49
      , 0x44
      , 0x33]

-- Tag that indicates version 2.3 of id3
v2 :: [Word8]
v2 = [ 0x02
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