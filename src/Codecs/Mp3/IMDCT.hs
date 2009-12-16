{-# LANGUAGE ForeignFunctionInterface #-}
-- 
-- module IMDCT
-- 
-- This code is part of the Experimental Haskell MP3 Decoder, version 0.0.1.
-- Copyright (c) 2008 Bjorn Edstrom <be@bjrn.se>
--
-- This software is provided 'as-is', without any express or implied
-- warranty. In no event will the authors be held liable for any damages
-- arising from the use of this software.
--
-- Permission is granted to anyone to use this software for any purpose,
-- including commercial applications, and to alter it and redistribute it
-- freely, subject to the following restrictions:
--
--    1. The origin of this software must not be misrepresented; you must not
--    claim that you wrote the original software. If you use this software
--    in a product, an acknowledgment in the product documentation would be
--    appreciated but is not required.
--
--    2. Altered source versions must be plainly marked as such, and must not be
--    misrepresented as being the original software.
--
--    3. This notice may not be removed or altered from any source
--    distribution.
--
module Codecs.Mp3.IMDCT (
    imdct
) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe

fi :: (Floating b, Integral a) => a -> b
fi = fromIntegral

foreign import ccall "c_imdct.h imdct"
    c_imdct :: CInt -> Ptr CDouble -> Ptr CDouble -> IO ()

imdctIO :: Int -> [CDouble] -> IO [CDouble]
imdctIO points input
    = withArray   input      $ \cinput ->
      allocaArray (points*2) $ \coutput ->
      do c_imdct (fromIntegral points) cinput coutput
         peekArray (points*2) coutput

imdct :: Int -> [Double] -> [Double]
imdct points input = let cinput  = map realToFrac input
                         coutput = unsafePerformIO (imdctIO points cinput)
                         output  = map realToFrac coutput
                     in output

--
-- Below are Haskell versions of the functions in c_imdct.c
-- translated in a naive way.
--

h_imdct18 :: [Double] -> [Double]
h_imdct18 xs = zipWith process xs lookup
  where
    process :: Double -> [Double] -> Double
    process k lkp = sum [ k * l | l <- lkp ]

    lookup :: [[Double]]
    lookup = [[ cos ( pi18 * (fi n + 9.5) * (fi k + 2.5))
             | k <- [0..17]] | n <- [0..35] ]
    pi18 = pi / 18.0

-- This one is the one described by wikipedia, maybe one
-- should stick with the one described by the C code?
h_imdct :: Int -> [Double] -> [Double]
h_imdct 18 xs = h_imdct18 xs
h_imdct x xs  = map (imdct_one bigN xs) [1..2*bigN]
  where
    bigN = length xs
    imdct_one :: Int -> [Double] -> Int -> Double
    imdct_one bigN xs n = invN * sum [ 
            sum [ x * cos (piN * (fi n + 0.5 + nhalf) * (fi k + 0.5))
                | k <- [0..bigN-1] ] 
                | x <- xs ]
      where
        invN  = 1 / (fi bigN)
        piN   = pi / (fi bigN)
        nhalf = (fi bigN) / 2
