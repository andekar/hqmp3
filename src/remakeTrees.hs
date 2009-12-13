module RemakeTrees where

import Data.List
import Control.Arrow
import Data.Array.IArray
import HuffTables
import Control.Monad (replicateM)

--
-- TODO: LookupHuff: BitGet?
--       Actual generation of trees
--       Testing
--

-- a may be (Int,Int) or (Int,Int,Int,Int)
type HuffTable a = Array Int (Int, a)
-- (CodeWordLength, Array)
type HuffArray a = (Int,HuffTable (Either a (HuffTable a)))

-- The type of huffman tables as used by Bjorn 
-- a may be (Int,Int) or (Int,Int,Int,Int)
type BjrnTable a = [([Int], a)]

-- Lookup an index in the array, get back value + bits to skip
-- Might want to make this in the BitGet monad instead?
lookupHuff :: HuffArray a -> Int -> (Int,a)
lookupHuff (cw,arr) i = case arr ! i of
    (l,Left a)    -> (l,a)
    (l,Right arr) -> let (l',a) = arr ! 0
                     in (l+l',a)

-- The "main" function
toArray :: BjrnTable a -> HuffArray a
toArray xs 
  = let (good,bad) = filterLength xs mean
        lowhigh   = map (first (splitAt mean)) bad
        grouped   = groupBy (\a b -> (fst . fst) a == (fst . fst) b) (sortForGroups lowhigh)
        grouped'  = map (\as -> (fst $ fst $ head as, map (\((a,b),c) -> (b,c)) as)) grouped
        grouped'' = map (second (Right . toArray')) grouped'
        allLists  = grouped'' ++ map (second Left) good
    in (mean, toArray' allLists)
  where
    mean = (sum $ map (length . fst) xs) `div` (length xs)
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
