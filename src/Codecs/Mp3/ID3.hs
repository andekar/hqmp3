{-# OPTIONS -w #-}
-- Comments in this file is partly taken from http://www.id3.org/id3v2.3.0
module ID3 ( -- functions
             skipId3
           , getId3v1
           , getId3v2_2
             -- constructors
           , ID3(..)
           , Version(..)
           , Frame (..)) where

import Debug.Trace
import Control.Monad

import qualified Data.ByteString as Bc
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Char
import Data.Bits
import Data.Maybe

-- TODO make this module run with (only) Strict.Get

import qualified Data.Binary.Strict.BitGet as G
import qualified BitGet as BG
import Data.Binary.Get

-- | A list of not supported frames
unsup :: [B.ByteString]
unsup = map B.pack 
        [ "AENC", "APIC", "COMR", "ENCR"
        , "ETCO", "GEOB", "GRID", "IPLS"
        , "MLLT", "POSS", "RVRB", "SYLT"
        , "SYTC" ]

data ID3 = 
      COMM String | ENQUA String
    | LINK String | MCDI String
    | OWNE String | PRIV String
    | PCNT String | POPM String
    | RBUF String | RVAD String
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
    | TCON String | TCOP String | TDAT String | TDLY String
    | TENC String | TEXT String | TFLT String | TIME String
    | TIT1 String | TIT2 String | TIT3 String | TKEY String
    | TLAN String | TLEN String | TMED String | TOAL String
    | TOFN String | TOLY String | TOPY String | TOPE String
    | TORY String | TOWN String | TPE1 String | TPE2 String
    | TPE3 String | TPE4 String | TPOS String | TPUB String
    | TRCK String | TRDA String | TRSN String | TRSO String
    | TSIZ String | TSRC String | TSSE String | TYER String
    deriving (Show, Eq, Read)

data Header = Header { version :: Maybe Version
                     , flags :: B.ByteString
                     , siz :: Word32
                     , body :: B.ByteString }
    deriving (Show, Eq, Read)

-- | The different id3 versions
data Version = ID3v1 | ID3v2 | ID3v2_2 | ID3v2_4
    deriving (Show, Eq, Read)

-- | A ID3 Frame contains flags, theese are not considered in this file!!
data Frame = Frame { id      :: B.ByteString
                   , fSize   :: Int
                   , flag    :: B.ByteString
                   , content :: B.ByteString }
    deriving (Eq, Read)

-- | This instance will make it easier to avoid parsing, instead we can use
-- read
instance Show Frame where
    show (Frame id _ _ c) = B.unpack id ++ " \"" ++ B.unpack c ++ "\""

-- | Produce a list of ID3 information instead of a list of frames containing
-- unneccessary information.
frameToData :: [Frame] -> [ID3]
frameToData = map (read . show)

-- | Simple test function
mains :: IO ()
mains = do content <- L.readFile "song.mp3"
           content' <- B.readFile "song.mp3"
           let (Just r) = runGet getId3v2_2 content
           print r
           return ()

-- | Get ID3v1
-- this will currently fail if a ID3v1.1 is used, the id3v1.1 has removed
-- 2bytes at each of the 30bytes fields to save space
getId3v1 :: Get (Maybe ( B.ByteString
                       , B.ByteString
                       , B.ByteString
                       , B.ByteString
                       , B.ByteString
                       , B.ByteString ))
getId3v1 = do rem     <- remaining
              skip (fromIntegral rem - 128)
              tag     <- getBytes 3
              title   <- getBytes 30
              artist  <- getBytes 30
              album   <- getBytes 30
              year    <- getBytes 3
              comment <- getBytes 30
              -- Check if last 2 bytes describe the track number
              genre   <- getBytes 1
              if tag == B.pack "TAG" then
                  return $ Just ( f tag, f title
                                , f artist, f album
                                , f year, f comment)
                  else return Nothing
    where f = Bc.filter (flip notElem [ 0x00 -- \NUL
                                      ])
          track bs = do --let bs' = Bc.unpack (drop 28 bs)
                        return Nothing
--                         case head bs' == 0x00 of
--                             True -> return Nothing
--                             _ -> return Nothing
--                      

-- | Skip an ID3 header and frames
-- first we peek at 3 bytes to check that it indeed is a Id3v2_* tag at the
-- beginning of the stream we are given. This function is lazy and might need
-- to be changes into a strict if we have speed issues
skipId3 :: BG.BitGet ()
skipId3 = do maybeTag <- BG.lookAhead checkTag
             case maybeTag of
                 Just x ->  do BG.skip (6*8) -- skip TAG, version and flags
                               size <- replicateM 4 (BG.getAsWord8 8)
                               let size' = byteSize size
                               BG.skip (size' * 8)
                               skipId3 -- as long as we find consecutive id3s
                 Nothing -> return ()

    where checkTag = do tag <- replicateM 3 (BG.getAsWord8 8)
                        case tag == map (fromIntegral . ord) "ID3" of
                            True  -> return $ Just tag
                            False -> return Nothing
          byteSize = fromIntegral . getSize . (map fromIntegral)

-- | Will if possible extract frames from an mp3 file that is given as a 
-- ByteString as input.
getId3v2_2 :: Get (Maybe [Frame])
getId3v2_2 = do tags <- getBytes 3
                ver  <- getBytes 2
                if (tags == tag &&
                    version' ver == Just ID3v2_2) then do
                        calID3 (version' ver)
                        else  return Nothing
    where calID3 v = do flags <- getBytes 1
                        size <- getBytes 4
                        let sizes = byteSize size
                        body <- getLazyByteString sizes
                        return $ Just $ runGet id3Frames body
          byteSize = fromIntegral . getSize . (map fromIntegral) . Bc.unpack

-- | Collect ID3 fGetid3Frames :: Get [Frame]
id3Frames
    = do empty <- isEmpty
         if empty then do
             return []
             else do
                 id      <- getBytes 4
                 size    <- getBytes 4
                 flags   <- getBytes 2
                 content <- getBytes $ s size
                 rest    <- id3Frames
                 if (notElem id unsup) then do
                     return (Frame id (s size) flags (filtNull content) : rest)
                     else do return rest
    where s size = fromIntegral . calcVal . map fromIntegral $ Bc.unpack size
          calcVal :: [Word32] -> Word32
          calcVal (x0:x1:x2:x3:[]) = let x0' = shiftL x0 24
                                         x1' = shiftL x1 16
                                         x2' = shiftL x2 8
                                     in x0' .|. x1' .|. x2' .|. x3

-- Filter all \NUL data
filtNull :: B.ByteString -> B.ByteString
filtNull r = Bc.filter (flip notElem [ 0x00 -- \NUL
                                 ]) r

-- | The first TAG indicating we have an id3 tag
tag :: B.ByteString
tag = Bc.pack [ 0x49
              , 0x44
              , 0x33]

-- | Checks which version of ID3 the file is using
version' :: B.ByteString -> Maybe Version
version' r | r == v2    = Just ID3v2
           | r == v2_2  = Just ID3v2_2
           | r == v2_4  = Just ID3v2_4
           | otherwise = Nothing
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
-- in this calcualtion the most significant bit is unused and should not be
-- considered in the calculation.
getSize :: [Word32] -> Word32
getSize (x0:x1:x2:x3:[])
    = let x0' = shiftL x0 21
          x1' = shiftL x1 14
          x2' = shiftL x2 7
          x3' = x3
      in x0' .|. x1' .|. x2' .|. x3'
getSize _ = error "Wrong argument to getSize"
