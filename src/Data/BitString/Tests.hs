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
    | begin+end >= 8 && L.length lb == 1 = False
    | L.null lb                          = False
    | otherwise                          = prop_invariant rest

prop_take :: Int -> BitString -> Bool
prop_take i bs = List.take i' bList == bisToList (take (fromIntegral i') bs)
   where
       bList = bisToList bs
       i' = fromIntegral $ abs i

prop_takeWord8 :: BitString -> Bool
prop_takeWord8 bs = List.take 8 bList
                  == bisToList (Chunk (L.pack [(takeWord8 bs)]) 0 0 Empty)
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

prop_head :: BitString -> QC.Property
prop_head Empty = True QC.==> True
prop_head bis = prop_invariant bis QC.==> 
                List.head (bisToList bis) == if (head bis) then 1 else 0

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

-- TODO fix these, numbers are sort of reversed
bisToList :: BitString -> [Int]
bisToList bs = nums
  where nums = btl bs

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

