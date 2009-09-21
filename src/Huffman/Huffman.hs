module Huffman where

import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.Word
import Data.List
import Data.PriorityQueue
import Control.Monad.ST

--main :: FilePath -> IO (HuffTree (Word8,Int)) 
main file = do
    f <- B.readFile file
    return $ create $ sortBy s $ M.toList $ analyze f
  where
    s :: (Word8,Int) -> (Word8,Int) -> Ordering
    s (_,i) (_,j) = compare i j

analyze :: B.ByteString -> M.Map Word8 Int
analyze b = B.foldr f M.empty b
  where
    f :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
    f w m = M.insertWith (+) w 1 m 

data (Eq a, Num b) => HuffTree a b =
    Leaf b a
  | Node b (HuffTree a b) (HuffTree a b)
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

create' :: (Eq a, Num b, Ord b) => PriorityQueue (ST s) (HuffTree a b) -> (ST s (HuffTree a b))
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

