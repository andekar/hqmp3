{-# LANGUAGE BangPatterns #-}

module Huffman ( analyze
               , HuffTree (..)
               , create
               , decode ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import Data.List
import Data.PriorityQueue
import Control.Monad.ST
import Data.Bits
import Data.Array.ST
import Data.Array

--main :: FilePath -> IO (HuffTree (Word8,Int)) 
main file = do
    f <- B.readFile file
    return $ create $ sortBy s $ M.toList $ analyze f
  where
    s :: (Word8, Int) -> (Word8, Int) -> Ordering
    s (_,i) (_,j) = compare i j

-- Can probably be speeded up (TODO)
analyze :: B.ByteString -> M.Map Word8 Int
analyze b = B.foldr f M.empty b
  where
    f :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
    f w m = M.insertWith (+) w 1 m 

data (Eq a, Num b) => HuffTree a b =
    Leaf !b !a
  | Node !b !(HuffTree a b) !(HuffTree a b)
    deriving Show

instance (Eq a, Num b) => Eq (HuffTree a b) where
    Leaf b a     == Leaf b' a'      = b == b && a == a'
    Node b t1 t2 == Node b' t1' t2' = b == b && t1 == t1' && t2 == t2'
    _            == _               = False

instance (Eq a, Ord b, Num b) => Ord (HuffTree a b) where
    t1 <= t2 = getVal t1 <= getVal t2

-- create :: (Eq a, Num b, Ord b) => [(a,b)] -> HuffTree a b
create xs = runST $ do
    q <- newPriorityQueue id
    mapM_ (\(e,i) -> enqueue q (Leaf i e)) xs
    create' q

create' :: (Eq a, Num b, Ord b) => PriorityQueue (ST s) (HuffTree a b)
           -> (ST s (HuffTree a b))
create' p = do
    (Just x1) <- dequeue p
    x         <- dequeue p
    case x of
        Nothing -> return x1
        Just x2 -> do
            enqueue p $ merge x1 x2
            create' p
  where
    merge :: (Eq a, Num b) => HuffTree a b -> HuffTree a b -> HuffTree a b
    merge x1 x2 = Node (getVal x1 + getVal x2) x1 x2

getVal (Leaf w _)   = w
getVal (Node w _ _) = w

-- | Decode takes the list of bits, an original tree, an recursed tree
-- an int describing which bit to check, an int describing the number of
-- padding bits.
decode :: (Num b, Eq a) =>
     [Word8] -> HuffTree a b -> HuffTree a b -> Int -> Int -> [a]

decode xs t (Leaf _ v) i j = v : decode xs t t i j
decode [] _ _ i j   = if i == j && j == 0 then [] else error "error"
decode (x:[]) t (Node _ t1 t2) i j
    | i == j = []
    | otherwise = decode [x] t (tree x i t1 t2) (i + 1) j
decode (x:xs) t (Node _ t1 t2) i j 
    | i == 7 = decode xs t (tree x i t1 t2) 0 j
    | otherwise = decode (x:xs) t (tree x i t1 t2) (i + 1) j

tree bit num t1 t2 = if testBit bit num then t2 else t1

tree2Map :: (Num b, Num c, Ord a) => HuffTree a b -> c -> M.Map a c -> M.Map a c
tree2Map (Leaf _ a) i  m    = M.insert a i m
tree2Map (Node _ t1 t2) i m = 
    let m' = tree2Map t1 (i * 2) m
    in tree2Map t2 (i * 2 + 1) m'
