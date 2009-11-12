module Tests where

import BitString
import Prelude hiding (null,head,tail,take,drop,concat,splitAt, length)
import Data.Word
import Control.Monad (liftM)
import Control.Arrow (first,second)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Test.QuickCheck as QC
import Data.Bits
import Data.Char
import Debug.Trace

instance QC.Arbitrary Word8 where
    arbitrary = do 
        i <- QC.choose (0,255) :: QC.Gen Int
        return $ fromIntegral i
    shrink i = map fromInteger $ QC.shrink (toInteger i)

instance QC.Arbitrary BitString where
    arbitrary = QC.sized $ \n -> QC.choose (0,n) >>= myArbitrary where 
        myArbitrary :: Int -> QC.Gen BitString
        myArbitrary 0 = return Empty
        myArbitrary n = do
            chunk <- QC.arbitrary
            rest  <- myArbitrary (n-1)
            begin <- liftM fromIntegral (QC.choose (0,7) :: QC.Gen Int)
            end   <- liftM fromIntegral (QC.choose (0,7) :: QC.Gen Int)
            return $ Chunk chunk begin end rest
    
    shrink Empty = []
    shrink (Chunk lb begin end rest) = 
        [rest] ++
        [(Chunk i begin end rest) | i <- QC.shrink lb   ] ++
        [(Chunk lb i end rest)    | i <- QC.shrink begin] ++
        [(Chunk lb begin i rest)  | i <- QC.shrink end  ] ++
        [(Chunk lb begin end i)   | i <- QC.shrink rest ]

instance QC.Arbitrary L.ByteString where
    arbitrary = QC.sized $ \n -> QC.choose (0,n) >>= myArbitrary where 
        myArbitrary :: Int -> QC.Gen L.ByteString
        myArbitrary 0 = return LI.Empty
        myArbitrary n = do
            ws  <- QC.arbitrary
            return $ L.pack ws

    shrink = map L.pack . QC.shrink . L.unpack

prop_invariant :: BitString -> Bool
prop_invariant Empty = True
prop_invariant (Chunk lb begin end rest)
    | begin + end >= 8 && L.length lb == 1 = False
    | begin > 7 = False
    | end > 7 = False
    | L.null lb                          = False
    | otherwise                          = prop_invariant rest

prop_take :: Int -> BitString -> QC.Property
prop_take i bis = prop_invariant bis QC.==> 
        List.take i' bList == bisToList (take (fromIntegral i') bis)
      && prop_invariant (take (fromIntegral i') bis)
    where
        bList = bisToList bis
        i' = fromIntegral $ abs i

-- this is broken
prop_takeWord8 :: BitString -> QC.Property
prop_takeWord8 bis = (prop_invariant bis && length bis >= 8 ) QC.==>
                    List.take 8 bList
                  == bisToList (Chunk (L.pack [takeWord8 bis]) 0 0 Empty)
    where bList = bisToList bis

-- wrongly specified
prop_takeAsWord8 :: Int -> BitString -> QC.Property
prop_takeAsWord8 i bis = prop_invariant bis QC.==>
                         List.take i (bisToList bis)
                         == bisToList (Chunk (L.pack [takeAsWord8 i bis]
                                             ) 0 0 Empty)

prop_drop :: Int -> BitString -> QC.Property
prop_drop i bis = prop_invariant bis QC.==>
                  List.drop (fromIntegral i') (bisToList bis)
                == bisToList fun
                && prop_invariant fun
    where i' = abs $ fromIntegral i
          fun = (drop i' bis)

prop_splitAt :: Int -> BitString -> QC.Property
prop_splitAt i bis = prop_invariant bis QC.==>
                      List.splitAt (fromIntegral i') (bisToList bis)
                   == lr bisToList
                   && tr (lr prop_invariant)
    where i' = abs $ fromIntegral i
          lr f= second f (first f (splitAt i' bis))
          tr (True,True) = True
          tr _  = False

prop_head :: BitString -> QC.Property
prop_head Empty = True QC.==> True
prop_head bis = prop_invariant bis QC.==>
                List.head (bisToList bis) == if (head bis) then 1 else 0

prop_tail :: BitString -> QC.Property
prop_tail bis = (prop_invariant bis && length bis > 0) QC.==>
                List.tail (bisToList bis) == bisToList (tail bis)
              && prop_invariant (tail bis)

prop_append :: BitString -> BitString -> QC.Property
prop_append bis bis' = prop_invariant bis && prop_invariant bis' QC.==>
                        (f bis) ++ (f bis')
                     ==  f fun
                     &&  prop_invariant fun
    where f = bisToList
          fun = append bis bis'

prop_concat :: [BitString] -> QC.Property
prop_concat biss = and (map prop_invariant biss) QC.==>
                   List.concat (map f biss)
                 == f (concat biss)
                 && prop_invariant (concat biss)
    where f = bisToList

prop_length :: BitString -> QC.Property
prop_length bs = prop_invariant bs QC.==> 
                 List.length (bisToList bs) == (fromIntegral $ length bs)

prop_atLeast :: Int -> BitString -> QC.Property
prop_atLeast i bs = prop_invariant bs && i >= 0 QC.==>
                    (List.length (bisToList bs) >= (fromIntegral i))
                        == atLeast bs (fromIntegral i)

prop_atLeastBS :: Int -> L.ByteString -> QC.Property
prop_atLeastBS i bs = i >= 0 QC.==>
                      ((L.length bs * 8) >= (fromIntegral i))
                    == atLeastBS bs (fromIntegral i)

-- TODO fix this, numbers are sort of reversed
-- also, how does one do when begin and/or end are not 0?
bisToList :: BitString -> [Int]
bisToList bs = nums
  where 
    nums = btl bs
    btl :: BitString -> [Int]
    btl Empty = []
    btl (Chunk lb begin end rest)
        | L.null lb = bisToList rest
        | begin+end == 8 && L.length lb == 1 = bisToList rest
        | begin == 7 = f bit : (bisToList $ Chunk (L.tail lb) 0 end rest)
        | otherwise  = f bit : (bisToList $ Chunk lb (begin+1) end rest)
      where
        bit = testBit (L.head lb) (fi begin)
        f True  = 1
        f False = 0

-- Recursive splitAt
splitAtMany :: Int -> [a] -> [[a]]
splitAtMany _ [] = []
splitAtMany i xs = f : splitAtMany i b
  where
    (f,b) = List.splitAt i xs

-- Shorthand for testing 500 times
largeTest :: (QC.Testable prop) => prop -> IO ()
largeTest prop = QC.quickCheckWith (QC.stdArgs { QC.maxSuccess = 500 }) prop
