module RemakeTrees where

import Data.List
import Control.Arrow
import Data.Array.IArray
import HuffTables
import Control.Monad (replicateM,liftM)
import Data.Binary.BitString.BitGet

-- We do not want to split every Array, since not all tables are big
type MP3Huffman a = Either (HuffTable a) (HuffArray a)
-- a may be (Int,Int) or (Int,Int,Int,Int)
type HuffTable a = Array Int (Int, a)
-- (CodeWordLength, Array)
type HuffArray a = (Int,HuffTable (Either a (HuffTable a)))
-- The type of huffman tables as used by Bjorn 
-- a may be (Int,Int) or (Int,Int,Int,Int)
type BjrnTable a = [([Int], a)]

-- Will output all huff-arrays for all trees, not coupled with linbits though.
-- Somebody please kill bjorn for his extremely stupid numbering system,
-- comments represent the number as used in ISO-11172-3
createTreesXY :: [MP3Huffman (Int,Int)]
createTreesXY = 
    [ Left $ toArray' tableHuffR00 -- table 1
    , Left $ toArray' tableHuffR01 -- table 2
    , Left $ toArray' tableHuffR02 -- table 3
    , Left $ toArray' tableHuffR03 -- table 5
    , Left $ toArray' tableHuffR04 -- table 6
    , Left $ toArray' tableHuffR05 -- table 7
    , Right $ toArray tableHuffR06 -- table 8
    , Left $ toArray' tableHuffR07 -- table 9
    , Right $ toArray tableHuffR08 -- table 10
    , Right $ toArray tableHuffR09 -- table 11
    , Right $ toArray tableHuffR10 -- table 12
    , Right $ toArray tableHuffR11 -- table 13
    , Right $ toArray tableHuffR12 -- table 15
    , Right $ toArray tableHuffR13 -- table 16
    , Right $ toArray tableHuffR14 -- table 24
    ]

-- The quadruple tables
createTreesVWXY :: [MP3Huffman (Int,Int,Int,Int)]
createTreesVWXY = 
    [ Left $ toArray' tableHuffRqa
    , Left $ toArray' tableHuffRqb]

-- Lookup a value in the huffman array
-- Second argument is length of the bitstream.
lookupHuff :: HuffArray a -> Int -> BitGet (Maybe (Int,a))
lookupHuff (cw,arr) length 
    | cw <= length = do
        i <- f cw
        case arr ! i of
            (l,Left a)     -> return $ Just (l,a)
            (l,Right arr') | l+cw <= length -> liftM (Just . (arr' !)) (f l)
                           | otherwise      -> return Nothing
    | otherwise = return Nothing
    
  where
    f w | w <= 8  = liftM fromIntegral (getAsWord8 $ fromIntegral w)
        | w <= 16 = liftM fromIntegral (getAsWord16 $ fromIntegral w)

-- The "main" function, for transforming trees to arrays
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
