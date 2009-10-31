module BitString where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.ByteString as S

import Data.Int
import Data.Bits
import Prelude hiding (drop, head, length, take, drop)
import Control.Monad
import qualified Data.List as List

data BitString = Empty | Chunk L.ByteString Int BitString

type Bit = Bool

empty :: BitString
empty = Empty

convert :: L.ByteString -> BitString
convert bs = Chunk bs 0 empty

convertAt :: L.ByteString -> Int64 -> BitString
convertAt bs i = Chunk (L.drop (i `div` 8) bs) (fromIntegral $ i `mod` 8) empty

readFile :: FilePath -> IO BitString
readFile = liftM convert . L.readFile

concat :: [BitString] -> BitString
concat = List.foldl append empty

append :: BitString -> BitString -> BitString
append Empty ys = ys
append (Chunk xs i rest) ys = Chunk xs i (append rest ys) 

index :: Int64 -> BitString -> Bool
index i bs = head $ drop i bs

head :: BitString -> Bool
head Empty = error "BitString.head: empty string"
head (Chunk bs i _) = testBit (L.head bs) i

-- how does one know that he really got that number of bits?
take :: Int64 -> BitString -> BitString
take i Empty = empty
take i (Chunk xs j rest) = undefined

drop :: Int64 -> BitString -> BitString
drop i Empty = empty
drop i (Chunk xs j rest)
    | atLeastBS xs (i - fromIntegral j) = undefined

null :: BitString -> Bool
null Empty = True
null _ = False

splitAt :: Int64 -> BitString -> (BitString, BitString)
splitAt i bs = (take i bs, drop i bs)

foldl :: (a -> Bool -> a) -> a -> BitString -> a
foldl = undefined

map :: (Bool -> Bool) -> BitString -> BitString
map = undefined

length :: BitString -> Int64
length Empty = 0
length (Chunk bs i rest) = (L.length bs * 8 - (fromIntegral i)) + length rest

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
    small = fromIntegral $ 8 - i
    sblen = small + (fromIntegral $ S.length sb) * 8

-- this function will under no circumstances be exported out of this module!!!
atLeastBS :: L.ByteString -> Int64 -> Bool
atLeastBS LI.Empty 0 = True
atLeastBS LI.Empty _ = False
atLeastBS (LI.Chunk sb lb) i 
    | i <= fromIntegral (S.length sb * 8) = True
    | otherwise = atLeastBS lb (i - (fromIntegral $ S.length sb) * 8)