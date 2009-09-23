{-# LANGUAGE BangPatterns #-}

module Main (test) where

--
-- This implementation of Huffman codes is designed to be very specific
-- and is suitable for when working with files, or other data that concists
-- of bytes.
--

import Data.Word (Word8)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST (ST)
import qualified Data.ByteString as B
import Data.Bits
import qualified Huffman as Huff
import qualified Data.Map as M
import Data.List

type HuffArray = Array Word8 (Word8,Word8)
type STHuff s  = STArray s Word8 (Word8,Word8)
type HuffTree  = Huff.HuffTree Word8 Int

main = test "test"

test :: FilePath -> IO ()
test file = do
    f <- B.readFile file
    let freqs = Huff.create $ Huff.analyze f
        arr  = tree2arr freqs
        res  = arr `seq` encode arr f
--        res' = Huff.decode (B.unpack res) analyzed analyzed 0 0
--        B.writeFile (file++".huff") res
    res `seq` print "hej" -- arr
 where
     s :: (Word8, Int) -> (Word8, Int) -> Ordering
     s (_,i) (_,j) = compare i j

-- This function takes a Huffman tree (that is the tree that contains the
-- compressed representations), and creates a kind of HashMap by using
-- the fact that Word8s are never larger than 255, and never smaller than 0
tree2arr :: HuffTree -> HuffArray
tree2arr t = runSTArray (do
                arr <- newArray_ (0, 255)
                walkTree t arr 0 0
                return arr)
  where
    walkTree :: HuffTree -> STHuff s -> Word8 -> Word8 -> ST s ()
    walkTree (Huff.Leaf _ w) arr i d     = writeArray arr w (i,d)
    walkTree (Huff.Node _ t1 t2) arr i d = do
        walkTree t1 arr (i * 2) (d+1)     -- Left branch
        walkTree t2 arr (i * 2 + 1) (d+1) -- Right branch

-- Only for testing, creates a tree of depth x
mkTree :: Word8 -> HuffTree
mkTree 0 = Huff.Leaf 0 3
mkTree x = Huff.Node 0 (mkTree (x - 1)) (mkTree (x - 1))

-- Wrapper for encode'
encode :: HuffArray -> B.ByteString -> B.ByteString
encode arr bs = B.pack $ encode' bs arr 0 8 0

-- i tells us where in the bytestring we are
-- j tells us where in i we are
encode' :: B.ByteString -> HuffArray -> Int -> Int -> Word8 -> [Word8]
encode' bs arr i j acc
    | B.length bs == i = [acc]
    | otherwise =
        let !(c,b) = arr ! (B.index bs i)
        in calcWords c (fromIntegral b)
  where
    calcWords c b
         | b < j = let acc' = acc .|. (c `shiftL` (j - b))
                   in  encode' bs arr (i + 1) (j - b) acc'
         | b == j = let acc' = acc .|. (c `shiftL` (j - b))
                   in  acc' : encode' bs arr (i + 1) 8 0
         | otherwise = let f = c `shiftR` (b - j)
                           r = c `shiftL` (8 - (b - j))
                       in  (acc .|. f) : encode' bs arr (i + 1) (8 - (b - j)) r
