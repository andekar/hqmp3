-- Comments in this file is partly taken from http://www.id3.org/id3v2.3.0
import qualified Data.ByteString as Bc
import qualified Data.ByteString.Char8 as B
import Data.Word
import Data.Bits
import Data.Maybe

-- | A list of not supported frames
unsup :: [B.ByteString]
unsup = map B.pack 
        [ "AENC"
        , "APIC"
        , "COMR"
        , "ENCR"
        , "ETCO"
        , "GEOB"
        , "GRID"
        , "IPLS"
        , "MLLT"
        , "POSS"
        , "RVRB"
        , "SYLT"
        , "SYTC" ]

data ID3 = 
      COMM String
    | ENQUA String
    | LINK String
    | MCDI String
    | OWNE String
    | PRIV String
    | PCNT String
    | POPM String
    | RBUF String
    | RVAD String
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

data Header = Header { version :: Version
                     , flags :: B.ByteString
                     , siz :: Word32
                     , body :: B.ByteString }
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

mains :: IO ()
mains = do content <- B.readFile "song.mp3"
           let (Just x) = getId3v2_2 content
           print x
           print $ frameToData x
           return () -- $ frameToData x

getId3v2_2 :: B.ByteString -> Maybe [Frame]
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
getID3v1 :: B.ByteString -> (  B.ByteString
                            , B.ByteString
                            , B.ByteString
                            , B.ByteString
                            , B.ByteString
                            , B.ByteString
                            , [Word8])
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
filt :: B.ByteString -> B.ByteString
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
      in if (notElem id unsup) then
            (Frame id sizeInt flags (filt content)):id3Frames bs'
            else id3Frames bs'
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

version' :: B.ByteString -> Version
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