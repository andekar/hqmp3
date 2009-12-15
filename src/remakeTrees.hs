module Main {- RemakeTrees -} where

import Data.List
import Control.Arrow
import Data.Array.IArray
import HuffTables
import HuffArrays
import Control.Monad (replicateM,liftM)

-- The type of huffman tables as used by Bjorn 
-- a may be (Int,Int) or (Int,Int,Int,Int)
type BjrnTable a = [([Int], a)]

main :: IO ()
main = do
    putStrLn "treesxy :: Array Int (Int,MP3HuffMan (Int,Int))"
    putStr   "treesxy = "
    putStrLn $ show createTreesXY
    putStrLn "treesvwxy = "
    putStrLn $ show createTreesVWXY

-- Will output all huff-arrays for all trees, coupled with linbits
-- Somebody please kill bjorn for his extremely stupid numbering system,
-- comments represent the number as used in ISO-11172-3
createTreesXY :: Array Int (Int, MP3Huffman (Int,Int))
createTreesXY = array (1, 31)
    [ (1,  (0, Left $ toArray' tableHuffR00))  -- table 1
    , (2,  (0, Left $ toArray' tableHuffR01))  -- table 2
    , (3,  (0, Left $ toArray' tableHuffR02))  -- table 3
    , (4,  (0, Left $ toArray' tableHuffR02))  -- table 4 is not used
    , (5,  (0, Left $ toArray' tableHuffR03))  -- table 5
    , (6,  (0, Left $ toArray' tableHuffR04))  -- table 6
    , (7,  (0, Right $ toArray tableHuffR05))  -- table 7
    , (8,  (0, Right $ toArray tableHuffR06))  -- table 8
    , (9,  (0, Right $ toArray tableHuffR07))  -- table 9
    , (10, (0, Right $ toArray tableHuffR08))  -- table 10
    , (11, (0, Right $ toArray tableHuffR09))  -- table 11
    , (12, (0, Right $ toArray tableHuffR10))  -- table 12
    , (13, (0, Right $ toArray tableHuffR11))  -- table 13
    , (14, (0, Right $ toArray tableHuffR11))  -- table 14 is not used
    , (15, (0, Right $ toArray tableHuffR12))  -- table 15
    , (16, (1, Right $ toArray tableHuffR13))  -- table 16
    , (17, (2, Right $ toArray tableHuffR13))  -- table 17
    , (18, (3, Right $ toArray tableHuffR13))  -- table 18
    , (19, (4, Right $ toArray tableHuffR13))  -- table 19
    , (20, (6, Right $ toArray tableHuffR13))  -- table 20
    , (21, (8, Right $ toArray tableHuffR13))  -- table 21
    , (22, (10, Right $ toArray tableHuffR13)) -- table 22
    , (23, (13, Right $ toArray tableHuffR13)) -- table 23
    , (24, (4, Right $ toArray tableHuffR14))  -- table 24
    , (25, (5, Right $ toArray tableHuffR14))  -- table 25
    , (26, (6, Right $ toArray tableHuffR14))  -- table 26
    , (27, (7, Right $ toArray tableHuffR14))  -- table 27
    , (28, (8, Right $ toArray tableHuffR14))  -- table 28
    , (29, (9, Right $ toArray tableHuffR14))  -- table 29
    , (30, (11, Right $ toArray tableHuffR14)) -- table 30
    , (31, (13, Right $ toArray tableHuffR14)) -- table 31
    ]

-- The quadruple tables
createTreesVWXY :: (MP3Huffman (Int,Int,Int,Int), MP3Huffman (Int,Int,Int,Int))
createTreesVWXY = (Left $ toArray' tableHuffRqa , Left $ toArray' tableHuffRqb)

-- The "main" function, for transforming trees to arrays
toArray :: BjrnTable a -> HuffArray a
toArray xs 
  = let (good,bad) = filterLength xs 8
        lowhigh   = map (first (splitAt 8)) bad
        grouped   = groupBy (\a b -> (fst . fst) a == (fst . fst) b) (sortForGroups lowhigh)
        grouped'  = map (\as -> (fst $ fst $ head as, map (\((a,b),c) -> (b,c)) as)) grouped
        grouped'' = map (second (Right . toArray')) grouped'
        allLists  = grouped'' ++ map (second Left) good
    in (8, toArray' allLists)
  where
--     mean = (sum $ map (length . fst) xs) `div` (length xs)
    filterLength xs l = partition (\a -> length (fst a) <= l) xs

-- Creates an array of size 2^n where n is the length of the longest code word
toArray' :: BjrnTable a -> HuffTable a
toArray' xs = let xs'   = concatMap (\(a,b) -> map (\(a',l) -> (l,(a',b))) $ allLists n a) xs
                  xsInt = map (first toInt) xs'
              in array (0,n') $ sortBy (\(a,_) (b,_) -> compare a b) xsInt
  where
    -- This is the length of the longest code word in the table
    n = foldl (\a entry -> max a (length $ fst entry)) 0 xs
    n' = 2^n - 1

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
