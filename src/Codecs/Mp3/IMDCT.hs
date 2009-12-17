module Codecs.Mp3.IMDCT (imdct) where

fi :: (Floating b, Integral a) => a -> b
fi = fromIntegral

-- Straightforward translation from the C code, elegant!
imdct18 :: [Double] -> [Double]
imdct18 xs = map (\tab -> sum $ zipWith (*) tab xs) lookupIMDCT
  where
    -- 36x18 matrix
    lookupIMDCT :: [[Double]]
    lookupIMDCT = [[ cos $ (pi / 18.0) * (fi n + 9.5) * (fi k + 0.5)
                  | k <- [0..17]] | n <- [0..35]]

-- Straightforward translation from the C code.
imdct :: Int -> [Double] -> [Double]
imdct 18 xs  = imdct18 xs
imdct pts xs = map (\n -> sum $ zipWith (subone n) xs [0..pts-1]) [0..2*pts-1]
  where
    subone :: Int -> Double -> Int -> Double
    subone n y k = y * (cos $ pipts * (fi n + 0.5 + nhalf) * (fi k + 0.5))
    pipts        = pi / (fi pts)
    nhalf        = (fi pts) / 2.0
