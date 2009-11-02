module BitString where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.ByteString as S

import Data.Int
import Data.Word
import Data.Bits
import Prelude hiding (drop, head, length, take, drop, splitAt, tail, concat)
import Control.Monad
import Control.Arrow
import qualified Test.QuickCheck as QC
import qualified Data.List as List

import Debug.Trace

data BitString = Empty | Chunk L.ByteString Int64 BitString
    deriving Show

-- oh lord...
instance QC.Arbitrary Word8 where
    arbitrary = do
        i <- QC.choose (0,255) :: QC.Gen Int
        return $ fromIntegral i

-- YEAH HASKELL!!!!
instance QC.Arbitrary BitString where
    arbitrary = do
        list <- QC.vector 100
        return $ convertWords list

instance QC.Arbitrary L.ByteString where
    arbitrary = do
        list <- QC.vector 100
        return $ L.pack list

-- Don't ever try to do this. It may cause permanent brain damage...
-- prop_take :: Int -> BitString -> Bool
-- prop_take i bis = List.take (fromIntegral i') bList == bisToList (take i' bis)
--     where bList = bisToList bis
--           i' = abs (fromIntegral i)

prop_take :: Int -> BitString -> Bool
prop_take i bs = List.take i' bList == bisToList (take (fromIntegral i') bs)
   where
       bList = bisToList bs
       i' = fromIntegral $ abs i

-- not working
prop_drop :: Int -> BitString -> Bool
prop_drop i bis = List.drop (fromIntegral i') (bisToList bis)
                == bisToList (drop i' bis)
    where i' = abs $ fromIntegral i

-- not working
prop_splitAt :: Int -> BitString -> Bool
prop_splitAt i bis = List.splitAt (fromIntegral i') (bisToList bis)
                   == second bisToList (first bisToList 
                                        (splitAt i' bis))
    where i' = abs $ fromIntegral i

prop_head :: BitString -> Bool
prop_head bis = List.head (bisToList bis) ==  if (head bis) then 1 else 0

prop_tail :: BitString -> Bool
prop_tail bis = List.tail (bisToList bis) == bisToList (tail bis)

prop_append :: BitString -> BitString -> Bool
prop_append bis bis' =  (f bis) ++ (f bis')
                     ==  f (append bis bis')
    where f = bisToList

prop_concat :: [BitString] -> Bool
prop_concat biss = List.concat (map f biss)
                 == f (concat biss)
    where f = bisToList

prop_atLeast :: Int -> BitString -> Bool
prop_atLeast i bis = ((List.length (f bis) * 8) >= (fromIntegral i))
                   == atLeast bis (fromIntegral i)
    where f = bisToList

prop_atLeastBS :: Int -> L.ByteString -> Bool
prop_atLeastBS i bs = ((L.length bs * 8) >= (fromIntegral i))
                    == atLeastBS bs (fromIntegral i)

bisToList :: BitString -> [Int]
bisToList Empty = []
bisToList ls = (f $ head ls) : (bisToList $ tail ls)
    where f True  = 1
          f False = 0

type Bit = Bool

empty :: BitString
empty = Empty

convert :: L.ByteString -> BitString
convert bs = Chunk bs 0 Empty

convertWords :: [Word8] -> BitString
convertWords ws = Chunk (L.pack ws) 0 Empty

convertAt :: L.ByteString -> Int64 -> BitString
convertAt bs i = Chunk (L.drop (i `div` 8) bs) (i `mod` 8) empty

readFile :: FilePath -> IO BitString
readFile = liftM convert . L.readFile

concat :: [BitString] -> BitString
concat = List.foldl append empty

append :: BitString -> BitString -> BitString
append Empty ys = ys
append (Chunk xs i rest) ys = Chunk xs i (append rest ys) 

index :: Int64 -> BitString -> Bit
index i bs = head $ drop i bs

head :: BitString -> Bit
head Empty = error "BitString.head: empty string"
head (Chunk bs i _) = testBit (L.head bs) (fromIntegral $ 7-i)

tail :: BitString -> BitString
tail Empty = error "BitString.tail: empty string"
tail bs = drop 1 bs

-- how does one know that he really got that number of bits?
take :: Int64 -> BitString -> BitString
take i bs = fst $ splitAt i bs

drop :: Int64 -> BitString -> BitString
drop i bs = snd $ splitAt i bs

null :: BitString -> Bool
null Empty = True
null _ = False

bitstr = Chunk (L.pack [0xA5, 0xff, 0xff]) 0 Empty

splitAt :: Int64 -> BitString -> (BitString, BitString)
splitAt i Empty = (Empty,Empty)
splitAt 0 r = (Empty,r)
splitAt i bs@(Chunk lb j rest)
    | atLeastBS lb (j + i)
    = let fst'  = L.take ((i + j + 7) `div` 8) lb 
          snd'  = L.drop ((i + j) `div` 8) lb
          sndch = if L.null snd'
                      then rest -- craazy stuff
                      else Chunk snd' ((i+j) `mod` 8) rest
          fst'' = if (j + rTrunc) >= 8 then L.tail $ trunced fst' 
                                      else if ((j+rTrunc) `mod` 8) == 0 then fst'
                                               else trunced fst'
      in (Chunk fst'' ((8 - ((i + j) `mod` 8)) `mod` 8) Empty, sndch) 

    -- These cases should work...
    | atLeast bs i' = let (f, s) = splitAt (i - (L.length lb - j)) rest
                      in  (Chunk lb j f, s)
    | otherwise = (bs, Empty)
  where
    i'  = i - (8 - j) 
    j' = 8 - (rTrunc + j)
    rTrunc = (i + j) `mod` 8
    trunced fst = rightShiftByteString (fromIntegral (8 - rTrunc)) fst

-- Taken from the BitGet library, just altered for lazy bytestrings
-- Not exported
rightShiftByteString :: Int -> L.ByteString -> L.ByteString
rightShiftByteString 0 = id
rightShiftByteString n = snd . L.mapAccumL f 0
 where
  f acc b = (b .&. (bottomNBits n), (b `shiftR` n) .|. (acc `shiftL` (8 - n)))
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

length :: BitString -> Int64
length Empty = 0
length (Chunk bs i rest) = (fromIntegral (L.length bs * 8) - i) + length rest

-- jeez...
atLeast :: BitString -> Int64 -> Bool
atLeast Empty 0 = True
atLeast Empty _ = False
atLeast (Chunk (LI.Chunk sb lb) i rest) j 
    | j <= small = True
    | j <= sblen = True
    | atLeastBS lb (j - sblen) = True
    | otherwise = atLeast rest (j - sblen)
  where
    small, sblen :: Int64
    small = 8 - i
    sblen = small + (fromIntegral $ S.length sb) * 8

-- this function will under no circumstances be exported out of this module!!!
atLeastBS :: L.ByteString -> Int64 -> Bool
atLeastBS LI.Empty 0 = True
atLeastBS LI.Empty _ = False
atLeastBS (LI.Chunk sb lb) i
    | i <= fromIntegral (S.length sb * 8) = True
    | otherwise = atLeastBS lb (i - (fromIntegral $ S.length sb) * 8)