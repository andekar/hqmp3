import Codec.Compression.Huffman.Huffman
import Data.List
import Control.Monad
import Data.Array.IArray
import Tables

-- Todo we must fix so that stuff get into the correct position
-- as of now the smallest element would be at position zero
toArray :: [([Int], (Int,Int))] -> Array Int (Int, (Int,Int))
toArray xs = let lists = allLists xs
                 list = listArray (0,(length lists - 1)) (map (\(_,y,z) -> (y,z)) (sorted lists))
             in list
    where sorted list = sortBy (\(x,_,_) (x',_,_) -> compare x x') (ls list)
          ls :: [([Int],a,b)] -> [(Int,a,b)]
          ls list = map (\(x,y,z) -> (toInt x, y ,z)) list
          toInt :: [Int] -> Int
          toInt xss  = fst $ foldl (\(a,v) b -> if b==1 then (a+v,v*2) else (a,v*2)) (0,1) (reverse xss)
                 
tableHuffR00 :: [([Int], (Int, Int))]
tableHuffR00 = [([1],(0,0)),
                ([0,0,1],(0,1)),
                ([0,1],(1,0)),
                ([0,0,0],(1,1))]

allLists :: [([Int],(Int,Int))] -> [([Int],Int,(Int,Int))]
allLists lists@((x,a):xs) = (concat $ map (all longest) lists)
    where longest = foldl (\x (y,_) -> max x (length y)) 0 lists
          all n (x,a)
              | n == length x = [(x,0,a)]
              | otherwise = (map (\vals -> (x++vals, rest, a)) (perm rest))
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