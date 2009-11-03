module BitString where

-- (c) Anders Karlsson
-- (c) Tobias Olausson

-- This library provides a pure interface to bits, similar to the monadic
-- BitGet library. This version uses lazy bytestrings.
-- All numeric arguments are in bits, not in bytes.

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.ByteString as S

import Data.Int
import Data.Word
import Data.Bits
import Prelude hiding (drop, head, length, take, drop, splitAt, tail, concat)
import Control.Monad (liftM)
import qualified Data.List as List

type Bit = Bool

-- This looks very much like the internals of ByteString.Lazy, with the added
-- Int64 that keeps track of bits in the first byte.
-- *invariant* If the BitString is a Chunk, then the bytestring in it is never
--  empty
data BitString = Empty | Chunk L.ByteString Int64 BitString
    deriving Show

-- Creates an empty BitString.
empty :: BitString
empty = Empty

-- Converts a Lazy ByteString to a BitString.
convert :: L.ByteString -> BitString
convert bs = Chunk bs 0 Empty

-- Converts and specifies a starting index, which may be larger than 8.
convertAt :: L.ByteString -> Int64 -> BitString
convertAt bs i = Chunk (L.drop (i `div` 8) bs) (i `mod` 8) empty

-- Similar to pack in ByteString
convertWords :: [Word8] -> BitString
convertWords ws = Chunk (L.pack ws) 0 Empty

-- Lazily reads a file into a BitString
readFile :: FilePath -> IO BitString
readFile = liftM convert . L.readFile

-- Concat a list of BitStrings
concat :: [BitString] -> BitString
concat = List.foldl append empty

-- Append a BitString onto another.
append :: BitString -> BitString -> BitString
append Empty ys = ys
append (Chunk xs i rest) ys = Chunk xs i (append rest ys) 

-- Indexing in a BitString gives a Bit
index :: Int64 -> BitString -> Bit
index i bs = head $ drop i bs

-- The first bit in the BitString
head :: BitString -> Bit
head Empty = error "BitString.head: empty string"
head (Chunk bs i _) = testBit (L.head bs) (fromIntegral $ 7 - i)

-- All but the first bit in the BitString
tail :: BitString -> BitString
tail Empty = error "BitString.tail: empty string"
tail bs = drop 1 bs

-- take is implemented as fst . splitAt
take :: Int64 -> BitString -> BitString
take i bs = fst $ splitAt i bs

takeWord8 :: BitString -> Word8
takeWord8 Empty = error "the universe might implode"
takeWord8 bs = L.head lb
    where (Chunk lb _ _) = take 8 bs

takeWord16 :: BitString -> Word16
takeWord16 Empty = error "oh no"
takeWord16 bs = (shiftL 8 $ wl lb) .|. wr lb
    where (Chunk lb _ _) = take 16 bs
          wl lb = fromIntegral $ L.head lb
          wr lb = fromIntegral $ L.head $ L.tail lb

takeAsWord8 :: Int64 -> BitString -> Word8
takeAsWord8 _ Empty = 0
takeAsWord8 i bis = let (Chunk r _ _) = take i bis
                    in fromIntegral $ L.head r

takeAsWord16 :: Int64 -> BitString -> Word16
takeAsWord16 _ Empty = 0
taleAsWord16 i bis = let (Chunk r _ _) = take i bis
                     in f r
    where f r = ((fromIntegral $ L.head r) `shiftL` 8) .|.
                (fromIntegral $ L.head $ L.tail r)

-- this will construct an rightshifted result!
getInt :: Int64 -> BitString -> Int
getInt _ Empty = 0
getInt i bs    = let (Chunk r _ _) = take i bs
                 in fromIntegral $ shiftBytes 0 $ map fromIntegral $ L.unpack r
    where shiftBytes :: Word32 -> [Word8] -> Word32
          shiftBytes w []     = w
          shiftBytes w (x:xs) = flip shiftBytes xs $ (shiftL w 8) .|. fi x
          fi :: Word8 -> Word32
          fi = fromIntegral
                     

-- drop is implemented as snd . splitAt
drop :: Int64 -> BitString -> BitString
drop i bs = snd $ splitAt i bs

-- Checks if a BitString is empty
null :: BitString -> Bool
null Empty = True
null _ = False

-- Splits a BitString into two parts.
-- This function is truly the most complicated in the library
splitAt :: Int64 -> BitString -> (BitString, BitString)
splitAt i Empty = (Empty, Empty)
splitAt 0 r     = (Empty, r)
splitAt i bs@(Chunk lb j rest)
    | atLeastBS lb (j + i) =
        let fst'  = L.take fstLen lb
            snd'  = L.drop sndLen lb
            sndch = if L.null snd'
                        then rest
                        else Chunk snd' rTrunc rest
            fst'' = if (j + rTrunc) >= 8
                    then L.tail $ trunced fst'
                    else if ((j+rTrunc) `mod` 8) == 0 then fst'
                                                      else trunced fst'
        in (Chunk fst'' ((8 - rTrunc) `mod` 8) Empty, sndch)

    -- These cases should work...NOT extensively tested though :(
    | atLeast bs (i - (8-j)) =
         let (f, s) = splitAt (i - (L.length lb - j)) rest
         in  (Chunk lb j f, s)
    | otherwise = (bs, Empty)
  where
    fstLen          = (i + j + 7) `div` 8
    (sndLen,rTrunc) = divMod (i + j) 8
    trunced fst = rightShiftByteString (fromIntegral (8 - rTrunc)) fst

-- Strictly checks the length of a BitString
length :: BitString -> Int64
length Empty = 0
length (Chunk bs i rest)
    = (fromIntegral (L.length bs * 8) - i) + length rest

-- Lazily checks if the BitString is at least j bits.
atLeast :: BitString -> Int64 -> Bool
atLeast Empty 0 = True
atLeast Empty _ = False
atLeast (Chunk (LI.Chunk sb lb) i rest) j
    | j <= sblen = True
    | atLeastBS lb (j - sblen) = True
    | otherwise = atLeast rest (j - sblen)
  where
    sblen :: Int64
    sblen = (8 - i) + (fromIntegral $ S.length sb) * 8

-- Functions below are not exported, but only used internally

-- this function will under no circumstances be exported out of this module!!!
atLeastBS :: L.ByteString -> Int64 -> Bool
atLeastBS LI.Empty 0 = True
atLeastBS LI.Empty _ = False
atLeastBS (LI.Chunk sb lb) i
    | i <= fromIntegral (S.length sb * 8) = True
    | otherwise = atLeastBS lb (i - (fromIntegral $ S.length sb) * 8)

-- Taken from the BitGet library, just altered for lazy bytestrings
rightShiftByteString :: Int -> L.ByteString -> L.ByteString
rightShiftByteString 0 = id
rightShiftByteString n = snd . L.mapAccumL f 0
 where
  f acc b = (b .&. (bottomNBits n), (b `shiftR` n) .|. (acc `shiftL` (8 - n)))

-- | Shift the whole ByteString some number of bits left where 0 <= @n@ < 8
leftShiftByteString :: Int -> L.ByteString -> L.ByteString
leftShiftByteString 0 = id
leftShiftByteString n = snd . L.mapAccumR f 0
    where
        f acc b = (b `shiftR` (8 - n), (b `shiftL` n) .|. acc)

-- | Return a Word8 with the bottom n bits set
bottomNBits :: Int -> Word8
bottomNBits 0 = 0
bottomNBits 1 = 0x01
bottomNBits 2 = 0x03
bottomNBits 3 = 0x07
bottomNBits 4 = 0x0f
bottomNBits 5 = 0x1f
bottomNBits 6 = 0x3f
bottomNBits 7 = 0x7f
bottomNBits 8 = 0xff
bottomNBits x = error ("bottomNBits undefined for " ++ show x)