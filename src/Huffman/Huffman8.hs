-- Copyright (c) Tobias Olausson 2009
-- Copyright (c) Anders Karlsson 2009

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Huffman8 ( encode
                , mkTree
                , tree2arr) where

--
-- This implementation of Huffman codes is designed to be very specific
-- and is suitable for when working with files, or other data that concists
-- of bytes.
--

import Data.Word (Word8)
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)
import Data.Bits
import qualified Huffman as Huff
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Bc
import Data.PriorityQueue

type HuffArray = Array Word8 (Word8,Word8)
type STHuff s  = STArray s Word8 (Word8,Word8)
type HuffTree  = Huff.HuffTree Word8

-- One would want to be able to use only the array
-- | The mkTree creates a Huffmantree out of a bytestring
-- the algorithm used for this is documented at
-- http://en.wikipedia.org/wiki/Huffman_encoding O(n log n) in algorithmic
-- performance. Note that this might not be the case in haskell depending on
-- structure used...
-- this function has some kind of an error inside!!!
mkTree :: B.ByteString -> HuffTree
mkTree b = runST $ do
    -- Analyze
    arr <- newArray (0, 255) 0 :: ST s (STUArray s Word8 Int)
    forM_ [0.. B.length b - 1] $ \i -> do
        let !w = B.index b i
        val <- readArray arr w
        writeArray arr w $! (val + 1)
    
    -- Create the tree
    q <- newPriorityQueue fst
    (l,h) <- getBounds arr
    forM_ [l..h] $ \e -> do
        !i <- readArray arr e
        if i /= 0 then enqueue q (i, Huff.Leaf e)
                  else return ()
    Huff.createTree q

-- | This function takes a Huffman tree (that is the tree that contains the
-- compressed representations), and creates a kind of HashMap by
-- the fact that Word8s are never larger than 255, and never smaller than 0
tree2arr :: HuffTree -> HuffArray
tree2arr t = runSTArray (do
                arr <- newArray_ (0, 255)
                walkTree t arr 0 0
                return arr)
  where
    walkTree :: HuffTree -> STHuff s -> Word8 -> Word8 -> ST s ()
    walkTree (Huff.Leaf w) arr i d     = writeArray arr w (i,d)
    walkTree (Huff.Node t1 t2) arr i d = do
        walkTree t1 arr (i * 2) (d + 1)     -- Left branch
        walkTree t2 arr (i * 2 + 1) (d + 1) -- Right branch

-- Wrapper for encode'
-- | The second in the tuple describes the number of "padding" bits
encode :: HuffArray -> B.ByteString -> (B.ByteString, Word8)
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

-- | The function to print a Hufftree into a list of the contents
printTree :: HuffTree -> B.ByteString
printTree t = case t of
    (Huff.Node t1 t2) ->
        B.append (B.append lParen (printTree t1))
        (printTree t2)
    (Huff.Leaf v) -> B.pack [v]
    where
        lParen :: B.ByteString
        lParen = Bc.pack "("

-- | The function to read a hufftree
readTree :: B.ByteString -> HuffTree
readTree = snd . readTree' 0
    where readTree' ::  Int -> B.ByteString -> (Int, HuffTree)
          readTree' pos str = case (head $ B.unpack $ B.take pos str) of
              0x28 -> 
                     let (pos', t1) = readTree' (pos' + 1) str
                         (pos'', t2) = readTree' pos' str
                     in (pos'', Huff.Node t1 t2)
              r      -> (pos + 1, Huff.Leaf r)