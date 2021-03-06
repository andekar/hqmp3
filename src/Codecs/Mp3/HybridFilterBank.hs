-- 
-- module HybridFilterBank - Frequency to time.
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
-- This code has been changed significantly

module Codecs.Mp3.HybridFilterBank (
    mp3HybridFilterBank
   ,MP3HybridState(..)
   ,MP3SynthState(..)
   ,emptyMP3HybridState
) where

-- Imports
import Codecs.Mp3.IMDCT
import Codecs.Mp3.SynthesisFilterBank
import Codecs.Mp3.Tables
import Codecs.Mp3.Types
import Data.Array.Unboxed
import Control.Monad.ST
import Data.Array.ST
import Control.Monad.Trans
import Control.Monad
import Data.List

-- Types
type STUDArr s = STUArray s Int Double
type STDArr s = ST s (STUDArr s)

-- we always have 576 samples so we do this 32 times (18*32)
mapBlock :: (UArray Int a -> (Int, Int) -> b) -> UArray Int a -> (Int, Int) -> [b]
mapBlock func seq (starts, end) = mb starts
  where mb start | start == end = []
                 | otherwise = func seq (start,start+18) : mb (start + 18)

-- 'windowWith' windows two signals. For readability, so we can do:
-- signal' = signal `windowWith` win
-- TODO: This should be at least one array?
windowWith :: (Num a) => [a] -> [a] -> [a]
windowWith = zipWith (*)

-- 
-- mp3IMDCT
--
-- Input: 576 frequency samples and state from the last time the function
-- was called.
-- Output: 576 time domain samples and new state.
--
mp3IMDCT :: BlockFlag -> Int -> UArray Int Frequency -> UArray Int Sample -> ([Sample], UArray Int Sample)
mp3IMDCT blockflag blocktype freq overlap =
    let (samples, overlap') = case blockflag of
             LongBlocks  -> transf (doImdctLong blocktype) freq (0,575)
             ShortBlocks -> transf doImdctShort freq (0,575)
             MixedBlocks -> let (first, second) = splitAt 36 (elems freq)
                            in transf (doImdctLong 0) freq (0,35) <++>
                               transf doImdctShort freq (36,575)
        samples' = zipWith (+) samples (elems overlap)
    in (samples', listArray (0,575) overlap')
    where
        transf f input = unzipConcat . mapBlock (toSO . f) input
        zipWith' :: [Int] -> UArray Int Sample -> (Int -> Sample -> Sample) -> [Sample]
        zipWith' = zips 0
        zips pointer [] _ _ = []
        zips pointer (x:xs) arr fun = let v1 = arr ! pointer
                                          res = fun x v1
                                      in (res : zips (pointer + 1) xs arr fun)

-- toSO takes 18 input samples b and computes 36 time samples
-- by the IMDCT. These are further divided into two equal
-- parts (S, O) where S are time samples for this frame
-- and O are values to be overlapped in the next frame.
toSO f b = splitAt 18 (f b)
unzipConcat xs = let (a, b) = unzip xs
                 in (concat a, concat b)

--
-- doImdctLong, doImdctShort
--
-- IMDCT with windows. This also does the overlapping when short blocks
-- are used.
--

-- The imdct long is quite easy if you compare with the short. We create 36
-- samples from the 18 we give this function.
doImdctLong :: Int -> UArray Int Frequency -> (Int, Int) -> [Sample]
doImdctLong blocktype f range = imdct18' f range `windowWith` tableImdctWindow blocktype

-- IMDCT short is like magic, you use some parts and then you glue them together
-- in a mysterious way. From 18 samples we create 36, in this case the first six
-- and the last six values will always be zero.
doImdctShort :: UArray Int Frequency -> (Int, Int) -> [Sample]
doImdctShort f (begin,end) = overlap3 shorta shortb shortc
  where
    shorta       = imdct6' f (begin, begin + 5) `windowWith` tableImdctWindow 2
    shortb       = imdct6' f (begin + 6, begin+11) `windowWith` tableImdctWindow 2
    shortc       = imdct6' f (begin + 12,begin+17) `windowWith` tableImdctWindow 2
    overlap3 a b c =
      -- length a b c = 12, a + b + c = 36
      -- left =
      -- (0-5) p1
      -- (6-11) 1/2 a
      -- (12-17) 1/2 a + 1/b
      -- right
      -- 0-5 1/2 b + 1/2 c
      -- 6-11 1/2 c
      -- 12-17 0
      p1 ++ (zipWith3 add3 (a ++ p2) (p1 ++ b ++ p1) (p2 ++ c)) ++ p1
      where
        add3 x y z = x+y+z
        p1         = [0,0,0, 0,0,0]
        p2         = [0,0,0, 0,0,0, 0,0,0, 0,0,0]

splitAt2 :: Int -> [a] -> ([a], [a], [a])
splitAt2 n xs = let (part1, part23) = splitAt n xs
                    (part2, part3)  = splitAt n part23
                in (part1, part2, part3)

(<++>) :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
(as, xs) <++> (bs, ys) = (as++bs, xs++ys)
infixr 5 <++>

-- 
-- mp3AA
-- Undo the encoders alias reduction.

-- This function uses real magic
-- DO NOT change it because that might cause madness
mp3AA :: BlockFlag -> Int -> STUDArr s -> STDArr s
mp3AA blockflag blocktype freq'
  | blocktype == 2 && blockflag /= MixedBlocks = return freq'
  | blocktype == 2 &&  blockflag == MixedBlocks = do
      aaHelper 9 27 freq'
      return freq'
  | otherwise = do
      aaHelper 9 567 freq'
      return freq'
  where
      aaHelper count range chunk | count < range
               = do aaButterfly (count + 1) chunk
                    aaHelper (count + 18) range chunk
                                 | otherwise = return chunk
      aaButterfly begin f = fixButter f fourTuples' (begin,begin + 15)
      fixButter arr tArr (begin, end)
          = do forM_ [0..7] $ \i -> do
                      first <- readArray arr (begin + i)
                      last  <- readArray arr (end - i)
                      let (f1, f2, f3, f4) = tArr ! i
                      writeArray arr (begin + i) ((first * f1) - (last * f2))
                      writeArray arr (end - i) ((first * f3) + (last * f4))
               return arr
      fourTuples = zip4 cs' ca' ca' cs'
      fourTuples' :: Array Int (Double, Double, Double, Double)
      fourTuples' = listArray (0,15) $ fourTuples

      cs = [1 / sqrt (1.0 + c**2) | c <- aaCoeff]
      ca = [c / sqrt (1.0 + c**2) | c <- aaCoeff]
      aaCoeff = [-0.6, -0.535, -0.33, -0.185, 
                 -0.095, -0.041, -0.0142, -0.0037]
      ca' = [c / sqrt (1.0 + c**2) | c <- aaCoeff']
      cs' = [1 / sqrt (1.0 + c**2) | c <- aaCoeff']
      aaCoeff' = [-0.0037, -0.0142, -0.041, -0.095,
                  -0.185, -0.33, -0.535, -0.6]

cs = [1 / sqrt (1.0 + c**2) | c <- aaCoeff]
ca = [c / sqrt (1.0 + c**2) | c <- aaCoeff]
{-# ANN revCs ([1 / sqrt (1.0 + c**2) | c <- [-0.6, -0.535, -0.33, -0.185, 
           -0.095, -0.041, -0.0142, -0.0037]] ++
        [1 / sqrt (1.0 + c**2) | c <-  [-0.6, -0.535, -0.33, -0.185, 
           -0.095, -0.041, -0.0142, -0.0037]]:: [Double]) #-}
revCs = [1 / sqrt (1.0 + c**2) | c <- aaCoeff] ++ cs
{-# ANN revCa ([c / sqrt (1.0 + c**2) | c <- [-0.0037, -0.0142, -0.041, -0.095,
        -0.185, -0.33, -0.535, -0.6]] ++
        [c / sqrt (1.0 + c**2) | c <-  [-0.6, -0.535, -0.33, -0.185, 
           -0.095, -0.041, -0.0142, -0.0037]]:: [Double]) #-}
revCa = [c / sqrt (1.0 + c**2) | c <- revAaCoeff] ++ ca
revAaCoeff = [-0.0037, -0.0142, -0.041, -0.095,
              -0.185, -0.33, -0.535, -0.6]
aaCoeff = [-0.6, -0.535, -0.33, -0.185, 
           -0.095, -0.041, -0.0142, -0.0037]

-- 
-- mp3FrequencyInvert
--
-- The time samples returned from mp3IMDCT are inverted. This is the same
-- as with bandpass sampling: odd subbands have inverteBd frequency spectra -
-- invert it by changing signs on odd samples.
--
mp3FrequencyInvert :: STUArray s Int Sample -> ST s (STUArray s Int Sample)
mp3FrequencyInvert arr  = zipWith' (*) pattern arr
    where pattern :: Array Int Double
          pattern = listArray (0,575) $ cycle $ replicate 18 1
                 ++ take 18 (cycle [1,-1])
          zipWith' fun a1 a2 = do forM_ [0..575] $ \i -> do
                                    let e1 = a1 ! i
                                    e2 <- readArray a2 i
                                    writeArray a2 i (fun e1 e2)
                                  return a2

-- Pads a list until it's length is n.
-- padWith 5 0 [1,2,3] == [1,2,3,0,0]
padWith :: Int -> a -> [a] -> [a]
padWith n padding xs = xs ++ replicate (n - length xs) padding

-- 
-- mp3HybridFilterBank
-- Frequency domain to time domain.
--
mp3HybridFilterBank :: BlockFlag -> Int -> 
                       MP3HybridState -> STUArray s Int Frequency ->
                       ST s (MP3HybridState, UArray Int Double)
mp3HybridFilterBank bf bt (MP3HybridState simdct ssynthesis) input =
    do aa'                   <- mp3AA bf bt input
       aa'' <- freeze aa'
    --         input'                = padWith 576 0.0 aa' -- ensure length 576
       let (samp, simdct')       = mp3IMDCT bf bt aa'' simdct
       samp'' <- newListArray (0,575) samp :: ST s (STUArray s Int Sample)
       sam''' <- mp3FrequencyInvert samp''
       (ssynthesis', output) <- mp3SynthesisFilterBank ssynthesis sam'''
       return (MP3HybridState simdct' ssynthesis', output)

-- [Sample] = IMDCT output from previous granule, used for overlapping.
-- MP3SynthState = State for the synthesis filterbank.
data MP3HybridState = MP3HybridState (UArray Int Double) MP3SynthState

emptyMP3HybridState :: MP3HybridState
emptyMP3HybridState
    = MP3HybridState (listArray (0,575) $ replicate 576 0.0)
                     (MP3SynthState (listArray (0,1023) empty))
  where empty = (replicate 1024 0.0)