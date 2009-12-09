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

-- For the Haskell version of IMDCT
import qualified Data.Array.ST as ST
import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed

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

-- This has a horribly long type irl, it has about one million type
-- variables which we don't bother typing in here...
h_imdct18 inArr outArr = do
    -- Lookup initilization
    lookupArr <- ST.newArray (0,17) 0 >>= \m -> ST.newArray (0,35) m
    forM_ [0..35] $ \n -> do
        forM_ [0..17] $ \k -> do
            innerArr <- ST.readArray lookupArr n
            ST.writeArray innerArr k $ initValue (fi n) (fi k)
    
    -- Here goes the actual calculations
    forM_ [0..35] $ \n -> do
        innerArr <- ST.readArray lookupArr n
        s <- forM [0,3..15] $ \k -> do
            v1 <- ST.readArray innerArr k
            v2 <- ST.readArray innerArr (k+1)
            v3 <- ST.readArray innerArr (k+2)
            let s0 = (inArr ! k)     + v1
                s1 = (inArr ! (k+1)) + v2
                s2 = (inArr ! (k+2)) + v3
            return $ s0 + s1 + s2
        ST.writeArray outArr n (sum s)
    return ()
  where
    initValue :: Double -> Double -> Double
    initValue n k = cos $ (pi / 18) * (n + 0.5 + 18/2.0) * (k + 0.5)

-- h_imdct :: Int -> UArray Int Double -> STUArray s Int Double -> ST s ()
h_imdct 18 inArr outArr     = h_imdct18 inArr outArr
h_imdct points inArr outArr = do
    let p = fi points
    forM_ [0..(points*2 - 1)] $ \n -> do
        s <- forM [0..(points-1)] $ \k -> 
            return $ (inArr ! k) 
                   * (cos $ (pi / p)
                          * (fi n + 0.5 + p/2.0)
                          * (fi k + 0.5))
        ST.writeArray outArr n (sum s)
    return ()
