--
-- This module is largely based on ideas taken from the experimental mp3 decoder
-- by Bjorn Edstrom. See LICENSE for details on this.
--

module Codecs.Mp3.SynthesisFilterBank (
     MP3SynthState(..)
    ,mp3SynthesisFilterBank
) where

import Data.Array.Unboxed as U
import Data.Array.ST as ST
import Control.Monad
import Control.Monad.ST
import Control.Arrow
import Codecs.Mp3.SynthesisTables

fi :: (Floating b, Integral a) => a -> b
fi = fromIntegral

type Sample = Double
newtype MP3SynthState = MP3SynthState (UArray Int Double)

-- will this be calculated at compiletime?
-- The lookup matrix used, 64x32
lookupSynth :: Array Int (Array Int Double)
lookupSynth = listArray (0,63) (map innerArrs [0..63])
  where innerArrs i = listArray (0,31) (map (val i) [0..31])
        val i j = cos $ (16.0 + i) * (2.0*j + 1) * (pi / 64.0)

mp3SynthesisFilterBank :: MP3SynthState -> [Sample] -> (MP3SynthState, UArray Int Double)
mp3SynthesisFilterBank (MP3SynthState oldstate) oldsamples
  = let samples = listArray (0,575) oldsamples
        newstates = stateList 0 oldstate samples
        output    = generateOutput newstates
    in  (MP3SynthState (fst $ last newstates), output)
  where stateList 18 _ _ = []
        stateList s state sample = let first = updateState s state sample
                                   in  (first, s*32) : stateList (s+1) first sample

updateState ::  Int -> UArray Int Double -> UArray Int Double -> UArray Int Double
updateState s oldstate samples = runSTUArray $ do
    newstate <- newArray_ (0,1023)
    forM_ [1023,1022..64] $ \i -> writeArray newstate i $ oldstate ! (i-64)
    forM_ [0..63]    $ \i -> do
        let r = sumZip updateVals s 0 (lookupSynth ! i) samples
        writeArray newstate i r
    return newstate
sumZip [] s accum a1 a2 = accum
sumZip ((x,y):xs) s accum a1 a2 =
             let v1 = a1 ! y
                 v2 = a2 ! (x + s)
                 accum' = accum + v1 * v2
             in  sumZip xs s accum' a1 a2

generateOutput :: [(UArray Int Double, Int)] -> UArray Int Double 
generateOutput states = runSTUArray $ do
        output <- newArray_ (0,575) :: ST s (STUArray s Int Double)
        uArr   <- newArray_ (0,575) :: ST s (STUArray s Int Double)
        forM_  states $ \(state,s) -> do
--             let state = states !! s
            toFast state uArr outList
            forM_ [0..511] $ \i -> do
                oldU <- readArray uArr i
                writeArray uArr i (oldU * synth_window ! i)
--             forM_ [0..31] $ \i -> do
--                 ss <- forM [0..15] $ \j -> readArray uArr (j*32 + i)
--                 writeArray output (32*s + i) (sum ss)

            forM_ outList2 $ \(i,i') -> do
                ss <- forM i $ \j -> readArray uArr j
                writeArray output (s + i') (sum ss)
        return output
    where toFast state _ [] = return ()
          toFast state arr (x:xs) = toFast' state arr x >> toFast state arr xs
          toFast' state _ [] = return ()
          toFast' state arr ((i, i', j, j'):xs) = do
              writeArray arr j (state ! i)
              writeArray arr j' (state ! i')
              toFast' state arr xs


              
          

