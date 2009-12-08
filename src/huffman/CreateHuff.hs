module CreateHuff where

import Huffman
import Tables

-- | This function creates a HuffTree from a "Table"
--   as given in Tables.hs. 
--   NOTE: This is an ugly hack (using dummy leaves temporarily)
table2Huff :: (Num t, Eq t1) => [([t], t1)] -> HuffTree t1 -> HuffTree t1
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

-- Autogenerate trees from tables
-- Creates the file HuffTrees.hs
main :: IO ()
main = do
    putStrLn "Quadruple tables:"
    print $ table2Huff tableHuffRqa (Leaf (-1,-1,-1,-1))
    print $ table2Huff tableHuffRqb (Leaf (-1,-1,-1,-1))
    putStrLn "Regular huffman tables:"
    print $ concatMap (++ "\n") trees
  where trees = flip map [0..14] $ \i ->
            ("tree" ++ show i ++ " = ") ++ (show $ table i)

-- Function calls to convert all tables into trees
table 0  = table2Huff tableHuffR00 (Leaf (-1,-1))
table 1  = table2Huff tableHuffR01 (Leaf (-1,-1))
table 2  = table2Huff tableHuffR02 (Leaf (-1,-1))
table 3  = table2Huff tableHuffR03 (Leaf (-1,-1))
table 4  = table2Huff tableHuffR04 (Leaf (-1,-1))
table 5  = table2Huff tableHuffR05 (Leaf (-1,-1))
table 6  = table2Huff tableHuffR06 (Leaf (-1,-1))
table 7  = table2Huff tableHuffR07 (Leaf (-1,-1))
table 8  = table2Huff tableHuffR08 (Leaf (-1,-1))
table 9  = table2Huff tableHuffR09 (Leaf (-1,-1))
table 10 = table2Huff tableHuffR10 (Leaf (-1,-1))
table 11 = table2Huff tableHuffR11 (Leaf (-1,-1))
table 12 = table2Huff tableHuffR12 (Leaf (-1,-1))
table 13 = table2Huff tableHuffR13 (Leaf (-1,-1))
table 14 = table2Huff tableHuffR14 (Leaf (-1,-1))
table _  = undefined
