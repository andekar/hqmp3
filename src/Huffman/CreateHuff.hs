module CreateHuff where

import Huffman
import Tables

table2Huff :: [([Int], (Int,Int))] -> HuffTree (Int, Int) -> HuffTree (Int, Int)
table2Huff [] t      = t
table2Huff (x:xs)  t = let t' = convert x t 
                       in table2Huff xs t'
    where convert ([], v) _ = Leaf v
          convert ((x:xs), v) (Node t1 t2)
              | x == 0 = Node (convert (xs,v) t1) t2
              | otherwise = Node t1 (convert (xs, v) t2)
          convert ((x:xs), v) t
              | x == 0 = Node (convert (xs, v) t) t
              | otherwise = Node t (convert (xs, v) t)

test = table2Huff tableHuffR00 (Leaf (-1,-1))