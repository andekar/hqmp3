module Codecs.Mp3.IMDCT (imdct, imdct18', imdct6') where
import Data.Array.Unboxed
import Control.Monad.ST
import Control.Monad
import Data.Array.ST
import Control.Monad.Trans
import Data.List
import System.IO.Unsafe

type STPair s = (STUArray s Int Double, STUArray s Int Double)
type TFun = (Double -> Double -> Double)

data Part = A | B | C
    deriving Eq

fi :: Int -> Double
fi = fromIntegral

-- Straightforward translation from the C code, elegant!
imdct18 :: [Double] -> [Double]
imdct18 xs = map (\s -> sumZip (*) 0.0 xs s) lookupIMDCT

{-# ANN lookupIMDCT ([[ cos $ (pi / 18.0) * n * k
                 | k <- [0.5, 1.5 .. 17.5]] | n <- [9.5, 10.5 .. 44.5]]
             :: [[Double]]) #-}
lookupIMDCT :: [[Double]]
lookupIMDCT = [[ cos $ (pi / 18.0) * n * k
                 | k <- [0.5, 1.5 .. 17.5]] | n <- [9.5, 10.5 .. 44.5]]
    
-- Instead of specialize
-- sumZip :: (Double -> Double -> Double) -> [Double] -> [Double] -> Double
sumZip f acc x1 x2 = lgo acc x1 x2
    where lgo acc [] _ = acc
          lgo acc _ [] = acc
          lgo acc (x:xs) (y:ys) = let res = acc + (f x y)
                                  in res `seq` lgo res xs ys

-- Straightforward translation from the C code.
imdct :: Int -> [Double] -> [Double]
imdct 18 xs  = imdct18 xs
imdct pts xs = map (\n -> sumZip (subone n) 0 xs [0..pts-1]) [0..2*pts-1]
  where
    subone :: Int -> Double -> Int -> Double
    subone n y k = y * (cos $ pipts * (fi n + 0.5 + nhalf) * (fi k + 0.5))
    pipts        = pi / (fi pts)
    nhalf        = (fi pts) / 2.0

{-
 - Until HybridFilterBank can handle arrays this code is useless, I'm afraid...
 -

-- Array version of the above, but is it any faster?
imdct18 :: UArray Int Double -> UArray Int Double
imdct18 xs = listArray (0,35) (map lookup [0..35])
  where
    -- This does the pairwise multiplication
    lookup :: Int -> Double
    lookup x = sum $ flip map [x*36 .. (x+1)*36-1] 
                   $ liftM2 (*) (lookupIMDCTArr !) (xs !)

-- Remains to be written
imdct :: Int -> UArray Int Double -> UArray Int Double
imdct 18 xs  = imdct18 xs
imdct pts xs = listArray (0,pts*2-1) $ map benny [0 .. 2*pts-1]
  where
    -- tribute to lucas jönefors
    benny :: Int -> Double
    benny n    = sum $ map (subone n) [0..pts-1]
    subone n k = let y = xs ! k
                 in y * (cos $ pipts * (fi n + 0.5 + nhalf) * (fi k + 0.5))
    nhalf = (fi pts) / 2.0
    pipts = pi / (fi pts)
-}

-- lookupIMDCT = listArray (0, 36*18-1) $ concat [[ cos $ (pi / 18.0) * n * k 
--   | k <- [0.5, 1.5 .. 17.5]] | n <- [9.5, 10.5 .. 44.5]]
-- 