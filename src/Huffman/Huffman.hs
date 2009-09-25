{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -Wall #-}

module Huffman ( analyze
               , HuffTree (..)
               , create
               , create'
               , decode ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import Data.PriorityQueue
import Control.Monad
import Control.Monad.ST
import Data.Bits

-- Can probably be speeded up (TODO)
analyze :: B.ByteString -> [(Word8,Int)]
analyze b = M.toList $ B.foldr f M.empty b
  where
    f :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
    f w m = M.insertWith (+) w 1 m

data (Eq a) => HuffTree a =
    Leaf !a
  | Node !(HuffTree a) !(HuffTree a)
    deriving (Show, Read)

instance (Eq a) => Eq (HuffTree a) where
    Leaf a     == Leaf a'      = a == a'
    Node t1 t2 == Node t1' t2' = t1 == t1' && t2 == t2'
    _            == _               = False

-- instance (Eq a) => Ord (HuffTree a) where
--     t1 <= t2 = getVal t1 <= getVal t2

-- create :: (Eq a, Num b, Ord b) => [(a,b)] -> HuffTree a b
create xs = runST $ do
    q <- newPriorityQueue fst
    mapM_ (\(e,i) -> enqueue q (i, (Leaf e))) xs
    create' q

create' :: (Eq a) => PriorityQueue (ST s) (Int, HuffTree a)
           -> (ST s (HuffTree a))
create' p = do
    (Just x1) <- dequeue p
    x         <- dequeue p
    case x of
        Nothing -> return $ snd x1
        Just x2 -> do
            enqueue p $ merge x1 x2
            create' p
  where
    merge :: Eq a => (Int, HuffTree a) -> (Int, HuffTree a) -> (Int, HuffTree a)
    merge (i, x1) (i', x2) = (i + i', Node x1 x2)

-- getVal (Leaf w _)   = w
-- getVal (Node w _ _) = w
 
-- Decodes a list of Words, using a Huffman tree
-- 
-- xs is the list of words
-- t is the huffman tree
-- the other tree is also a huffman tree, recursed on
-- i is the current bit in the current byte, sort of 
-- j is how many padding bits this stream has
--
-- Please do not change this code, checks are not checked
decode :: (Eq a) =>
     [Word8] -> HuffTree a -> HuffTree a -> Int -> Int -> [a]

decode xs t (Leaf v) i j = v : decode xs t t i j
decode [] _ _ i j   = if i == j && j == 0 then [] else error "error"
decode (x:[]) t (Node t1 t2) i j
    | i+1 == j = [] 
    | otherwise = decode [x] t (tree x i t1 t2) (i - 1) j
decode (x:xs) t (Node t1 t2) i j
    | i == 0 = decode xs t (tree x i t1 t2) 7 j
    | otherwise = decode (x:xs) t (tree x i t1 t2) (i - 1) j

tree bits num t1 t2 = if testBit bits num then t2 else t1
