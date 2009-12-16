module Main {- RemakeTrees -} where

import Data.List
import Control.Arrow
import Data.Array.IArray
import HuffTables
import Codecs.Mp3.HuffArrays
import Control.Monad (replicateM,liftM)

-- The type of huffman tables as used by Bjorn 
-- a may be (Int,Int) or (Int,Int,Int,Int)
type BjrnTable a = [([Int], a)]

-- Typical usage: ./remakeTrees >> HuffArrays.hs
main :: IO ()
main = do
    putStrLn "-- XY Tables below..."
    mapM_ output (zip ([1..3] ++ [5..13] ++ [15,16,24]) createTreesXY)
  where
    output (num, tree) = putStrLn $ "table"++(show num)++ " = " ++ (show tree)

-- Comments represent the number as used in ISO-11172-3
-- The first five (1-6) are shallow, whereas the other are deep
-- All tables with longest codeword <= 11 are not cut.
createTreesXY :: [MP3Huffman (Int,Int)]
createTreesXY =
    [ Left $ toArray' tableHuffR00  -- table 1
    , Left $ toArray' tableHuffR01  -- table 2
    , Left $ toArray' tableHuffR02  -- table 3
    , Left $ toArray' tableHuffR03  -- table 5
    , Left $ toArray' tableHuffR04  -- table 6
    , Left $ toArray' tableHuffR05  -- table 7
    , Left $ toArray' tableHuffR06  -- table 8
    , Left $ toArray' tableHuffR07  -- table 9
    , Left $ toArray' tableHuffR08  -- table 10
    , Left $ toArray' tableHuffR09  -- table 11
    , Left $ toArray' tableHuffR10  -- table 12
    , Right $ toArray tableHuffR11 10 -- table 13
    , Right $ toArray tableHuffR12 8  -- table 15
    , Right $ toArray tableHuffR13 9  -- table 16
    , Right $ toArray tableHuffR14 8  -- table 24
    ]

-- The quadruple tables
createTreesVWXY :: [MP3Huffman (Int,Int,Int,Int)]
createTreesVWXY = [Left $ toArray' tableHuffRqa , Left $ toArray' tableHuffRqb]

toArray :: BjrnTable a -> Int -> HuffDeep a
toArray xs cut
  = let (good,bad) = filterLength xs cut
        lowhigh   = map (first (splitAt cut)) bad
        grouped   = groupBy (\a b -> (fst . fst) a == (fst . fst) b) (sortForGroups lowhigh)
        grouped'  = map (\as -> (fst $ fst $ head as, map (\((a,b),c) -> (b,c)) as)) grouped
        innerArrs = map (\(num,val) -> (toInt num , Right $ toArray' val)) grouped'
        good'     = concatMap (\(a,b) -> map (\(a',l) -> (l,(a',b))) $ allLists cut a) good
        plainArrs = map (\(num,val) -> (toInt num , Left val)) good'
    in (cut, array (0,2^cut - 1) $ innerArrs ++ plainArrs)
  where
    filterLength xs l = partition (\a -> length (fst a) <= l) xs
    
-- Creates an array of size 2^n where n is the length of the longest code word
-- TODO: Give variables better names in the concatMap
toArray' :: BjrnTable a -> HuffArray a
toArray' xs = let xs'   = concatMap (\(a,b) -> map (\(a',l) -> (l,(a',b))) $ allLists n a) xs
                  xsInt = map (first toInt) xs'
              in (n,array (0,2^n - 1) $ sortBy (\(a,_) (b,_) -> compare a b) xsInt)
  where
    -- This is the length of the longest code word in the table
    n = foldl (\a entry -> max a (length $ fst entry)) 0 xs

-- Get all lists of length n that starts with xs
allLists :: Int -> [Int] -> [(Int,[Int])]
allLists i xs | i <= j    = [(j,xs)]
              | otherwise = zipWith (\x p -> (j,x++p)) (replicate pl xs) ps
  where
    ps = perm (i-j)
    pl = length ps
    j  = length xs

-- toInt [0,1,0,1] == 5
toInt :: [Int] -> Int
toInt xss  = fst $ foldl (\(a,v) b -> if b==1 then (a+v,v*2) 
                          else (a,v*2)) (0,1) (reverse xss)

sortForGroups :: Ord a => [((a,b),c)] -> [((a,b), c)]
sortForGroups xs = sortBy sorts xs
    where sorts ((a,b),c) ((a',b'),c') = compare a a'

-- 2^n lists of length n, lol @ code
perm :: Int -> [[Int]]
perm x = replicateM x [0,1]
