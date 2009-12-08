-- Copyright (c) Tobias Olausson 2009
-- Copyright (c) Anders Karlsson 2009

module Codec.Compression.Huffman.Huffman ( analyze
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
import Data.Binary.BitString.BitGet
import Data.Int

-- Can probably be speeded up (TODO)
-- | Analyze will create a list of Word8 tupled with the number of occurrences
-- in the given ByteString
analyze :: B.ByteString -> [(Word8,Int)]
analyze = M.toList . B.foldr f M.empty
  where
    f :: Word8 -> M.Map Word8 Int -> M.Map Word8 Int
    f w = M.insertWith (+) w 1

-- | The Huffmantree is a usual tree in haskell
data HuffTree a =
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
    Just x1 <- dequeue p -- The queue will allways have at least 1 element 
    x       <- dequeue p -- or we are in big troubles elsewhere!!!
    case x of
        Nothing -> return $ snd x1 -- If the list only contains one element
        Just x2 -> do              -- we are finished and can return.
            enqueue p $ merge x1 x2
            createTree p
  where
    -- merge simply concats 2 subtrees
    merge :: Eq a => (Int, HuffTree a) -> (Int, HuffTree a) -> (Int, HuffTree a)
    merge (i, x1) (i', x2) = (i + i', Node x1 x2)

-- The Huffman decoder.
-- Might be a target for optimization
decode :: Eq a => HuffTree a -> (a -> BitGet a) -> Int64 -> BitGet (Maybe a)
decode t f p = decode' t
  where
    decode' (Leaf v) = liftM Just (f v) 
    decode' (Node left right) = do
        r <- getAtLeast $ fromIntegral (p + 1)
        if not r then return Nothing
             else do
                b <- getBit
                if b then decode' right
                     else decode' left
