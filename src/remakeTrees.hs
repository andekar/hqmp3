import Codec.Compression.Huffman.Huffman
import Data.List
import Control.Monad
import Control.Arrow
import Data.Array.IArray
import Tables

type HuffEither = Either (Int,Int) (Array Int (Int,(Int,Int)))

-- toArray :: [([Int],(Int,Int))] -> Array Int (Int, HuffEither)
toArray xs 
  = let (good,bad) = filterLength xs mean
        lowhigh   = map (first (splitAt mean)) bad
        grouped   = groupBy (\a b -> (fst . fst) a == (fst . fst) b) lowhigh
        grouped'  = map (\as -> (fst $ fst $ head as, map (\((a,b),c) -> (b,c)) as)) grouped
        grouped'' = map (second (Right . toArray' (0,0))) grouped'
        allLists  = grouped'' ++ map (second Left) good
    in grouped -- allLists -- toArray' (Left (0,0)) allLists

  where
    mean = (sum $ map (length . fst) xs) `div` (length xs)
    filterLength xs l = partition (\a -> length (fst a) < l) xs


-- Todo we must fix so that stuff get into the correct position
-- as of now the smallest element would be at position zero
toArray' :: a -> [([Int], a)] -> Array Int (Int, a)
toArray' zval xs = let (lists,l') = allLists xs
                       l = 2^l' -1
                       list = listArray (0,l)
                              (insertEmpty l 0 zval (sorted lists))
             in list
    where sorted list = sortBy (\(x,_,_) (x',_,_) -> compare x x') (ls list)
          ls :: [([Int],a,b)] -> [(Int,a,b)]
          ls list = map (\(x,y,z) -> (toInt x, y ,z)) list
          toInt :: [Int] -> Int
          toInt xss  = fst $ foldl (\(a,v) b -> if b==1 then (a+v,v*2) 
                                    else (a,v*2)) (0,1) (reverse xss)

longest lists = foldl (\x (y,_) -> max x (length y)) 0 lists

insertEmpty :: Int -> Int -> a -> [(Int, Int, a)] -> [(Int, a)]
insertEmpty len curr zval []
    | curr < len = replicate (len - curr) (0,zval)
    | otherwise = []
insertEmpty len curr zval ((x,m,a):xs)
    | curr < x = replicate (x-curr) (0,zval)
            ++ [(m,a)] ++ insertEmpty len (x+1) zval xs
    | x == curr = (m,a):insertEmpty len (curr + 1) zval xs
    | otherwise = error "should maybe not happen"

allLists :: [([Int],a)] -> ([([Int],Int,a)], Int)
allLists lists@((x,a):xs) = ((concat $ map (all longest) lists), longest)
    where longest = foldl (\x (y,_) -> max x (length y)) 0 lists
          all n (x, a)
              | n == length x = [(x, 0, a)]
              | otherwise = (map (\vals -> (x ++ vals, length x, a)) (perm rest))
             where rest = n - length x

allCom :: Int -> [Int] -> [[Int]]
allCom long x = map (x ++) (perm long)

permute :: [a] -> [[a]]
permute str = rotate str len len
   where len = length str

rotate :: [a] -> Int -> Int -> [[a]]
rotate _ _ 0 = []
rotate s 1 _ = [s]
rotate (ch:chs) len rcnt =
   map (\x -> ch : x) (rotate chs (len-1) (len-1))
   ++
   rotate (chs ++ [ch]) len (rcnt-1)

perm :: Int -> [[Int]]
perm 0 = []
perm 1 = [[0],[1]]
perm x = (map (1:) $ perm (x-1)) ++ (map (0:) $ perm (x-1))
