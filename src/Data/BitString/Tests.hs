module Tests where

import BitString
import Prelude hiding (head,tail,take,drop,concat,splitAt, length)
import Data.Word
import Control.Monad (liftM)
import Control.Arrow (first,second)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as L
import qualified Test.QuickCheck as QC

instance QC.Arbitrary Word8 where
    arbitrary = do 
        i <- QC.choose (0,255) :: QC.Gen Int
        return $ fromIntegral i

instance QC.Arbitrary BitString where
    arbitrary = liftM convertWords $ QC.vector 100 --This is quite slow

instance QC.Arbitrary L.ByteString where
    arbitrary = liftM L.pack $ QC.vector 100

prop_take :: Int -> BitString -> Bool
prop_take i bs = List.take i' bList == bisToList (take (fromIntegral i') bs)
   where
       bList = bisToList bs
       i' = fromIntegral $ abs i

prop_takeWord8 :: BitString -> Bool
prop_takeWord8 bs = List.take 8 bList
                  == bisToList (Chunk (L.pack [(takeWord8 bs)]) 0 Empty)
    where bList = bisToList bs

prop_drop :: Int -> BitString -> Bool
prop_drop i bis = List.drop (fromIntegral i') (bisToList bis)
                == bisToList (drop i' bis)
    where i' = abs $ fromIntegral i

prop_splitAt :: Int -> BitString -> Bool
prop_splitAt i bis = List.splitAt (fromIntegral i') (bisToList bis)
                   == second bisToList (first bisToList 
                                        (splitAt i' bis))
    where i' = abs $ fromIntegral i

prop_head :: BitString -> Bool
prop_head bis = List.head (bisToList bis) ==  if (head bis) then 1 else 0

prop_tail :: BitString -> Bool
prop_tail bis = List.tail (bisToList bis) == bisToList (tail bis)

prop_append :: BitString -> BitString -> Bool
prop_append bis bis' =  (f bis) ++ (f bis')
                     ==  f (append bis bis')
    where f = bisToList

prop_concat :: [BitString] -> Bool
prop_concat biss = List.concat (map f biss)
                 == f (concat biss)
    where f = bisToList

prop_length :: BitString -> Bool
prop_length bs = List.length (bisToList bs) == (fromIntegral $ length bs)

prop_atLeast :: Int -> BitString -> Bool
prop_atLeast i bs = (List.length (bisToList bs) >= (fromIntegral i))
                        == atLeast bs (fromIntegral i)

prop_atLeastBS :: Int -> L.ByteString -> Bool
prop_atLeastBS i bs = ((L.length bs * 8) >= (fromIntegral i))
                    == atLeastBS bs (fromIntegral i)

bisToList :: BitString -> [Int]
bisToList Empty = []
bisToList ls = (f $ head ls) : (bisToList $ tail ls)
    where f True  = 1
          f False = 0

