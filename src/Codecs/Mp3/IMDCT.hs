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

-- #6->#12
imdct6 :: [Double] -> [Double]
imdct6 xs = map (\s -> sumZip (*) 0.0 xs s) lookupIMDCT6

imdct18' :: UArray Int Double -> (Int, Int) -> [Double]
imdct18' xs (begin, end) = map (\s -> sumZip'' 17 (*) 0.0 (begin + 17) xs s) lookupIMDCT

imdct6' :: UArray Int Double -> (Int, Int) -> [Double]
imdct6' xs (begin, end) = map (\s -> sumZip'' 5 (*) 0.0 (begin+5) xs s) lookupIMDCT6

-- A very specialized imdct6
-- We provide two mutable arrays, one for the state and one for output
-- the imdctshort is a bit special, which is why we do it in this way...
-- we still need to change this slightly so that we add some of the values
imdct6'' :: Part -> UArray Int Double -> STPair s -> (Int, Int) -> ST s (STPair s)
imdct6'' p xs st@(p1,p2) (begin, end) = do foldM_ fun 0 lookupIMDCT6
                                           return st
    where fun i n
              = case p of
                  A -> do write (6 + i) (maps n)
                          return (i + 1)
                  B -> do write (i + 12) (maps n)
                          return (i + 1)
                  C -> do write (i + 18) (maps n)
                          return (i + 1)
          write n | n < 18    = writeArray p1 (n + begin) -- first part
                  | otherwise = writeArray p2 (n + begin) -- second part
          maps = sumZip'' 5 (*) 0.0 (begin + 5) xs

{-# ANN lookupIMDCT6 ([[ cos $ (pi / 6.0) * (n + 0.5 + 6.0) * (k + 0.5)
                 | k <- [0 .. 5]] | n <- [0 ..11]]
             :: [[Double]]) #-}
lookupIMDCT6 :: [[Double]]
lookupIMDCT6 = [[ cos $ (pi / 6.0) * (n + 0.5 + 6.0) * (k + 0.5)
                  | k <- [0 .. 5]] | n <- [0 .. 11]]


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
                                  in  res `seq` lgo res xs ys

sumZip'' :: Int -> (Double -> Double -> Double) -> Double -> Int -> UArray Int Double -> [Double] -> Double
sumZip'' c f acc begin x1 x2 = lgo acc x1 x2 c
    where lgo acc _ _ (-1) = acc
          lgo acc xs (y:ys) c' = let res =  acc + (f (xs ! (begin - c')) y)
                                 in  res `seq` lgo res xs ys (c' -1)

sumZip' :: Int -> TFun -> Int -> UArray Int Double -> [Double] -> Double
sumZip' c f begin x1 x2 = lgo 0.0 x1 x2 c
    where lgo acc _ _ (-1) = acc
          lgo acc xs (y:ys) c' = let res =  acc + (f (xs ! (begin - c')) y)
                                 in  res `seq` lgo res xs ys (c' -1)


-- Straightforward translation from the C code.BB
imdct :: Int -> [Double] -> [Double]
imdct 18 xs  = imdct18' (listArray (0,35) xs) (0,17)


{-
 - Until HybridFilterBank can handle arrays this code is useless, I'm afraid...
 -

-- Array version of the above, but is it any faster?
-- imdct18'' :: UArray Int Double -> [Double]
-- imdct18'' xs = map lookup [0..35]
--   where
--     -- This does the pairwise multiplication
--     lookup :: Int -> Double
--     lookup x = sum $ flip map [x*36 .. (x+1)*36-1] 
--                    $ liftM2 (*) (lookupIMDCT' !) (xs !)

-- -- Remains to be written
-- imdct' :: Int -> UArray Int Double -> UArray Int Double
-- imdct' 18 xs  = imdct18 xs
-- imdct' pts xs = listArray (0,pts*2-1) $ map benny [0 .. 2*pts-1]
--   where
--     -- tribute to lucas jönefors
--     benny :: Int -> Double
--     benny n    = sum $ map (subone n) [0..pts-1]
--     subone n k = let y = xs ! k
--                  in y * (cos $ pipts * (fi n + 0.5 + nhalf) * (fi k + 0.5))
--     nhalf = (fi pts) / 2.0
--     pipts = pi / (fi pts)
-- -- -}

-- lookupIMDCT' = listArray (0, 36*18-1) $ concat [[ cos $ (pi / 18.0) * n * k | k <- [0.5, 1.5 .. 17.5]] | n <- [9.5, 10.5 .. 44.5]]
