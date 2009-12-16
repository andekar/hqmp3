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

module Codecs.Mp3.IMDCT (imdct) where

{-
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Array
import System.IO.Unsafe
-}

fi :: (Floating b, Integral a) => a -> b
fi = fromIntegral

{-
 - The C calls are kept here for historical reasons, and in case
 - somethings goes very bad...
 -
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

-}

--
-- Below are Haskell versions of the functions in c_imdct.c
--

-- Straightforward translation from the C code, elegant!
imdct18 :: [Double] -> [Double]
imdct18 xs = zipWith (\k -> sum . map (k*)) xs lookupIMDCT
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
