-- Copyright (c) Tobias Olausson 2009
-- Copyright (c) Anders Karlsson 2009

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Huffman ( analyze
               , HuffTree (..)
               , create
               , createTree
               , decode ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import Data.PriorityQueue
import Control.Monad
import Control.Monad.ST
import Data.Bits

-- Can probably be speeded up (TODO)
-- | Analyze will create a list of Word8 tupled with the number of occurrences
-- in the given ByteString
analyze :: B.ByteString -> [(Word8,Int)]
analyze b = M.toList $ B.foldr f M.empty b
  where
    f :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
    f w m = M.insertWith (+) w 1 m

-- | The Huffmantree is an usual tree in haskell
data Eq a => HuffTree a =
      Leaf !a 
    | Node !(HuffTree a) !(HuffTree a)
  deriving (Show, Read)

create :: Eq a => [(a, Int)] -> HuffTree a
create xs = runST $ do
    q <- newPriorityQueue fst
    mapM_ (\(e,i) -> enqueue q (i, (Leaf e))) xs
    createTree q

createTree :: Eq a => PriorityQueue (ST s) (Int, HuffTree a)
              -> (ST s (HuffTree a))
createTree p = do
    (Just x1) <- dequeue p -- The queue will allways have at least 1 element 
    x         <- dequeue p -- or we are in big troubles elsewhere!!!
    case x of
        Nothing -> return $ snd x1 -- If the list only contains one element
        Just x2 -> do              -- we are finished and can return.
            enqueue p $ merge x1 x2
            createTree p
  where
    -- merge simply concats 2 subtrees
    merge :: Eq a => (Int, HuffTree a) -> (Int, HuffTree a) -> (Int, HuffTree a)
    merge (i, x1) (i', x2) = (i + i', Node x1 x2)

-- | Decodes a list of Words, using a Huffman tree
-- 
-- xs is the list of words
-- t is the huffman tree
-- the other tree is also a huffman tree, recursed on
-- i is the current bit in the current byte, sort of 
-- j is how many padding bits this stream has
--
-- Please do not change this code, checks are not checked
decode :: Eq a => [Word8] -> HuffTree a -> [a]
decode bs ts = decode' bs ts ts 7 0
    where decode' :: Eq a => [Word8] -> HuffTree a -> HuffTree a
                     -> Int -> Int -> [a]
          decode' xs t (Leaf v) i j = v : decode' xs t t i j
          decode' [] _ _ i j   = if i == j && j == 0 then [] else error "error"
          decode' (x:[]) t (Node t1 t2) i j
              | i+1 == j = [] 
              | otherwise = decode' [x] t (tree x i t1 t2) (i - 1) j
          decode' (x:xs) t (Node t1 t2) i j
              | i == 0 = decode' xs t (tree x i t1 t2) 7 j
              | otherwise = decode' (x:xs) t (tree x i t1 t2) (i - 1) j
          tree :: Bits b => b -> Int -> HuffTree a -> HuffTree a -> HuffTree a
          tree bits num t1 t2 = if testBit bits num then t2 else t1
