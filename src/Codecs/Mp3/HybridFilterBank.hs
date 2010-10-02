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

mapBlock :: Int -> ([a] -> b) -> [a] -> [b]
mapBlock blocksize func []  = []
mapBlock blocksize func seq =
    let (block, rem) = splitAt blocksize seq
    in func block : mapBlock blocksize func rem

-- experimental
-- we always have 576 samples so we do this 32 times (18*32)
mapBlock' :: (UArray Int a -> (Int, Int) -> b) -> UArray Int a -> (Int, Int) -> [b]
mapBlock' func seq (starts, end) = mb starts
--     let (block, rem) = splitAt 18 seq
--     in func block : mapBlock' 18 func rem
  where mb start | start == end = []
                 | otherwise = func seq (start,start+18) : mb (start + 18)
-- experimental --

-- 'windowWith' windows two signals. For readability, so we can do:
-- signal' = signal `windowWith` win
windowWith :: (Num a) => [a] -> [a] -> [a]
windowWith = zipWith (*)

-- 
-- mp3IMDCT
--
-- Input: 576 frequency samples and state from the last time the function
-- was called.
-- Output: 576 time domain samples and new state.
--
mp3IMDCT :: BlockFlag -> Int -> [Frequency] -> [Sample] -> ([Sample], [Sample])
mp3IMDCT blockflag blocktype freq overlap =
    let (samples, overlap') = case blockflag of
             LongBlocks  -> transf (doImdctLong blocktype) freq
             ShortBlocks -> transf (doImdctShort) freq
             MixedBlocks -> let (first, second) = splitAt 36 freq 
                            in transf (doImdctLong 0) first <++>
                               transf (doImdctShort)  second
        samples' = zipWith (+) samples overlap
    in (samples', overlap')
    where
        transf imdctfunc input = unzipConcat $ mapBlock 18 toSO input
            where
                -- toSO takes 18 input samples b and computes 36 time samples
                -- by the IMDCT. These are further divided into two equal
                -- parts (S, O) where S are time samples for this frame
                -- and O are values to be overlapped in the next frame.
                toSO b = splitAt 18 (imdctfunc b)
                unzipConcat xs = let (a, b) = unzip xs
                                 in (concat a, concat b)

--
-- doImdctLong, doImdctShort
--
-- IMDCT with windows. This also does the overlapping when short blocks
-- are used.
--

doImdctShort :: [Frequency] -> [Sample]
doImdctShort f = overlap3 shorta shortb shortc
  where
    (f1, f2, f3) = splitAt2 6 f
    shorta       = imdct 6 f1 `windowWith` tableImdctWindow 2
    shortb       = imdct 6 f2 `windowWith` tableImdctWindow 2
    shortc       = imdct 6 f3 `windowWith` tableImdctWindow 2
    
    overlap3 a b c = 
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
-- TODO: Rewrite this, clumsy and non-intuitive.
--

-- experimental
-- This function uses real magic
-- DO NOT change it because that might release the hellhounds
mp3AA' :: BlockFlag -> Int -> STUDArr s -> STDArr s
mp3AA' blockflag blocktype freq'
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

-- experimental --
mp3AA :: BlockFlag -> Int -> [Frequency] -> [Frequency]
mp3AA blockflag blocktype freq
  | blocktype == 2 && blockflag /= MixedBlocks   = freq
  | blocktype == 2 && blockflag == MixedBlocks
    = let (first, second) = splitAt 9 freq
          (third, fourth) = splitAt 18 second
      in first ++ aaHelper third ++ fourth
  | otherwise
    = let (first, second) = splitAt 9 freq
          (third, fourth) = splitAt 558 second
      in first ++ aaHelper third ++ fourth
  where
      aaHelper []    = []
      aaHelper (c:chunk) = c : (aaButterfly middle ++ [after] ++ 
                                aaHelper last)
          where
              (middle, (after:last)) = splitAt 16 chunk
      aaButterfly f = let (seqcs', seqcs'') = splitAt 8 seqcs
                          (seqca', seqca'') = splitAt 8 seqca
                      in zipWith (-) seqcs' seqca' ++
                         zipWith (+) seqcs'' seqca''
          where
              seqcs :: [Double]
              seqcs = zipWith (*) f revCs
              seqca :: [Double]
              seqca = reverse $ zipWith (*) f revCa

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
-- as with bandpass sampling: odd subbands have inverted frequency spectra -
-- invert it by changing signs on odd samples.
--
mp3FrequencyInvert :: [Sample] -> [Sample]
mp3FrequencyInvert = zipWith (*) pattern
    where pattern = cycle $ replicate 18 1 ++ take 18 (cycle [1,-1])

-- Pads a list until it's length is n.
-- padWith 5 0 [1,2,3] == [1,2,3,0,0]
padWith :: Int -> a -> [a] -> [a]
padWith n padding xs = xs ++ replicate (n - length xs) padding

-- 
-- mp3HybridFilterBank
-- Frequency domain to time domain.
--
mp3HybridFilterBank :: BlockFlag -> Int -> 
                       MP3HybridState -> [Frequency] -> 
                       (MP3HybridState, UArray Int Double)
mp3HybridFilterBank bf bt (MP3HybridState simdct ssynthesis) input =
    let -- aa                    = mp3AA bf bt input
        aa'                   = runST (cast input bf bt)
        input'                = padWith 576 0.0 aa' -- ensure length 576
        (samp, simdct')       = mp3IMDCT bf bt input' (elems simdct)
        samp'                 = mp3FrequencyInvert samp
        (ssynthesis', output) = mp3SynthesisFilterBank ssynthesis samp'
    in (MP3HybridState (listArray (0,575) simdct') ssynthesis', output)

-- [Sample] = IMDCT output from previous granule, used for overlapping.
-- MP3SynthState = State for the synthesis filterbank.
data MP3HybridState = MP3HybridState (UArray Int Double) MP3SynthState

-- cast :: [Double] -> [Double]
cast arr bf bt = do arr' <- newListArray (0,575) arr
                    res  <- mp3AA' bf bt arr'
                    getElems res

emptyMP3HybridState :: MP3HybridState
emptyMP3HybridState
    = MP3HybridState (listArray (0,575) $ replicate 576 0.0)
                     (MP3SynthState (listArray (0,1023) empty))
  where empty = (replicate 1024 0.0)