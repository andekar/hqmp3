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
import Data.List

fi :: (Floating b, Integral a) => a -> b
fi = fromIntegral

type Sample = Double
newtype MP3SynthState = MP3SynthState (UArray Int Double)

-- TODO: Change to Mutable array
mp3SynthesisFilterBank :: MP3SynthState -> UArray Int Sample -> ST s (MP3SynthState, UArray Int Double)
mp3SynthesisFilterBank (MP3SynthState oldstate) oldsamples
  = do let -- samples = listArray (0,575) oldsamples
           newstates = stateList updateVals' oldstate oldsamples
           output    = generateOutput newstates
       return  (MP3SynthState (fst $! last newstates), output)
  where stateList [] _ _ = []
        stateList ((x',x):xs) state sample
            = let first = updateState x state sample
              in  first `seq` ((first, x') : stateList xs first sample)

updateState ::  [(Int,Int)] -> UArray Int Double -> UArray Int Double -> UArray Int Double
updateState updateVals oldstate samples = runSTUArray $ do
    newstate <- newArray_ (0,1023)
    mapM_ (\(i,j) -> writeArray newstate i $! oldstate ! j) updateList
    forM_ [0..63]    $ \i -> do
        let r = sumZip updateVals (lookupSynth ! i) samples
        writeArray newstate i r
    return newstate

sumZip xs a1 a2 = foldl' helper 0 xs
    where helper accum (x,y) = let v1 = a1 ! y
                                   v2 = a2 ! x
                               in  accum + v1 * v2

generateOutput :: [(UArray Int Double, Int)] -> UArray Int Double 
generateOutput states = runSTUArray $ do
        output <- newArray_ (0,575) :: ST s (STUArray s Int Double)
        uArr   <- newArray_ (0,575) :: ST s (STUArray s Int Double)
        forM_  states $ \(state,s) -> do
            toFast state uArr outList
            forM_ [0..511] $ \i -> do
                oldU <- readArray uArr i
                writeArray uArr i (oldU * synth_window ! i)
            forM_ outList2 $ \(i,i') -> do
                ss <- forM i $ \j -> readArray uArr j
                writeArray output (s + i') (foldl' (+) 0 ss)
        return output
    where toFast state _ [] = return ()
          toFast state arr (x:xs) = toFast' state arr x >> toFast state arr xs
          toFast' state _ [] = return ()
          toFast' state arr ((i, i', j, j'):xs) = do
              writeArray arr j (state ! i)
              writeArray arr j' (state ! i')
              toFast' state arr xs
