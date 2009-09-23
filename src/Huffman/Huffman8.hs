module HuffMan8 where

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

type HuffArray = UArray Word8 Word8
data HuffTree = 
      Leaf Word8
    | Node HuffTree HuffTree

-- This function takes a Huffman tree (that is the tree that contains the
-- compressed representations), and creates a kind of HashMap by using
-- the fact that Word8s are never larger than 255, and never smaller than 0
tree2arr :: HuffTree -> HuffArray
tree2arr t = runSTUArray (do
                arr <- newArray_ (0, 255)
                walkTree t arr 0
                return arr)
  where
    walkTree :: HuffTree -> STUArray s Word8 Word8 -> Word8 -> ST s ()
    walkTree (Leaf w) arr i     = writeArray arr i w
    walkTree (Node t1 t2) arr i = do
        walkTree t1 arr (i * 2)     -- Left branch
        walkTree t2 arr (i * 2 + 1) -- Right branch

-- Only for testing, the argument should be between 0 and 8
mkTree :: Word8 -> HuffTree
mkTree 0 = Leaf 3
mkTree x = Node (mkTree (x - 1)) (mkTree (x - 1))

encode :: HuffArray -> B.ByteString -> B.ByteString
encode arr bs = B.pack $ encode' bs arr 0 0 0

-- i tells us where in the bytestring we are
-- j tells us where in i we are
encode' :: B.ByteString -> HuffArray -> Int -> Int -> Word8 -> [Word8]
encode' bs arr i j acc
    | B.length bs == i = []
    | otherwise =
        let c = arr ! (B.index bs i)
            b = bits c
        in allaFlickor c b
  where
    allaFlickor c b
        | b < j = let acc' = acc .|. (c `shiftL` (j - b))
                  in  encode' bs arr (i + 1) (j + b) acc'
        | b == j = let acc' = acc .|. (c `shiftL` (j - b))
                  in  acc' : encode' bs arr (i + 1) 0 0
        | otherwise = let f = c `shiftR` (b - j)
                          r = c `shiftL` (8 - (b-j))
                      in  (acc .|. f) : encode' bs arr (i + 1) (8 - (b - j)) r
    -- Check how many bits some number require
    bits w
        | w > 127   = 8
        | w > 63    = 7
        | w > 31    = 6
        | w > 15    = 5
        | w > 7     = 4
        | w > 3     = 3
        | w > 1     = 2
        | otherwise = 1
