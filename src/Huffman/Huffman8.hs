{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Main where

--
-- This implementation of Huffman codes is designed to be very specific
-- and is suitable for when working with files, or other data that concists
-- of bytes.
--

import Data.Word (Word8)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST (ST)
import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.Bits
import qualified Huffman as Huff
import Data.List
import System.Environment

type HuffArray = Array Word8 (Word8,Word8)
type STHuff s  = STArray s Word8 (Word8,Word8)
type HuffTree  = Huff.HuffTree Word8 Int

main = do
    [m,f] <- getArgs
    case m of
        "encode" -> encoder f
        "decode" -> decoder f

decoder :: FilePath -> IO ()
decoder file = do
    treeFile <- readFile $ file++".tree"
    let !tree = read treeFile :: HuffTree
    enc <- B.readFile file
    let res = Huff.decode (B.unpack enc) tree tree 7 0
    print res

encoder :: FilePath -> IO ()
encoder file = do
    f <- B.readFile file
    f `seq` print "Read file"
    let freqs  = Huff.create $ analyze f
        arr  = tree2arr freqs
        (res, pad)  = encode arr f
    B.writeFile (file ++ ".huff") res
    writeFile (file ++ ".huff.tree") (show freqs)

-- This should be a faster version
analyze :: B.ByteString -> [(Word8,Int)]
analyze b = filter ((0 /=) . snd) $ assocs $ runSTUArray $ do
    arr <- newArray (0, 255) 0 
    forM_ [0.. B.length b - 1] $ \i -> do
        let !w = B.index b i
        val <- readArray arr w
        writeArray arr w $! (val + 1)
    return arr

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
        walkTree t1 arr (i * 2) (d + 1)     -- Left branch
        walkTree t2 arr (i * 2 + 1) (d + 1) -- Right branch

-- Wrapper for encode'
-- the second in the tuple describes the number of "padding" bits
encode :: HuffArray -> B.ByteString -> (B.ByteString,Word8)
encode arr bs = (B.init bs', B.last bs')
  where
    bs' = B.pack $ encode' bs arr 0 8 0

-- i tells us where in the bytestring we are
-- j tells us what is left in i
encode' :: B.ByteString -> HuffArray -> Int -> Int -> Word8 -> [Word8]
encode' bs arr i j acc
      -- add the number of bits that is padding
    | B.length bs == i = acc : [fromIntegral j]
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
