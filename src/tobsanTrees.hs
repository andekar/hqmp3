module Main where

import Control.Arrow
import Data.List
import Data.Array
import Tables

-- a may be (Int,Int) or (Int,Int,Int,Int)
type HuffTable a = Array Int (Int, a)
type HuffArray a = Array Int (Either a (HuffTable a))

-- The type of huffman tables as used by Bjorn 
-- a may be (Int,Int) or (Int,Int,Int,Int)
type BjrnTable a = [([Int], a)]

toArray :: BjrnTable a -> HuffArray a
toArray xs = 
    let (long,short) = partition (\a -> length (fst a) < mean) xs
        shortArray   = toArray' short
        -- TODO the ones that are long
    in undefined
  where
    -- mean length of the code words in table
    mean = (sum $ map (length . fst) xs) `div` (length xs)

-- TODO: Make the concatMap look better please
-- Creates an array of size 2^n where n is the length of the longest code word
toArray' :: BjrnTable a -> HuffTable a
toArray' xs = let xs'   = concatMap (\(a,b) -> map (\(a',l) -> (l,(a',b))) $ allLists n a) xs
                  xsInt = map (first toInt) xs'
              in array (0,n') $ sortBy (\(a,_) (b,_) -> compare a b) xsInt
  where
    -- This is the length of the longest code word in the table
    n = foldl (\a entry -> max a (length $ fst entry)) 0 xs
    n' = 2^n - 1
    

-- toInt [0,1,0,1] == 5
toInt :: [Int] -> Int
toInt xs = fst $ foldl (\(a,m) b -> if b==1 then (a+m,m*2) else (a,m*2)) (0,1) 
                       (reverse xs)

-- Get all lists of length n that starts with xs
allLists :: Int -> [Int] -> [(Int,[Int])]
allLists i xs
    | i <= j    = [(j,xs)]
    | otherwise = zipWith (\x p -> (j,x++p)) (replicate pl xs) ps
  where
    ps = perm (i-j)
    pl = length ps
    j  = length xs

-- Creates all possible binary lists of length n
perm :: Int -> [[Int]]
perm 0 = []
perm 1 = [[0],[1]]
perm x = (map (1:) $ perm (x-1)) ++ (map (0:) $ perm (x-1))
