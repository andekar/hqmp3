-- 
-- module PCMWriter - For writing PCM WAV files.
-- This is a small simple (and very SLOW) library for demonstrating
-- the decoder. For more powerful manipulations of PCM
-- WAV, see HSoundFile at Hackage.
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

module PCMWriter (
      writeHeader
    , writeSamplerate
    , writeSamples
    , writeNumSamples
) where

import Data.Word
import Data.Bits
import System.IO
import Data.Char
import Control.Monad
import Control.Monad.State

type PCMSample = Word16

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fracToPCM :: RealFrac a => a -> PCMSample
fracToPCM x
    | y <= (-32767)   = fi (-32767)
    | y >= 32767      = fi 32767
    | otherwise       = fi y
    where
        y = round $ (x * 32767.0)
{-# SPECIALIZE fracToPCM :: Double -> PCMSample #-}
{-# SPECIALIZE fracToPCM :: Float -> PCMSample #-}
{-# INLINE fracToPCM #-}

class AudioSampleRepr a where
    toPcmRepr   :: a -> PCMSample

instance AudioSampleRepr Double where
    toPcmRepr = fracToPCM

instance AudioSampleRepr Float where
    toPcmRepr = fracToPCM

-- Add other instances here.

from16bit :: Word16 -> [Char]
from16bit n = [chr . fromIntegral $ n .&. 0xff,
               chr . fromIntegral $ n `shiftR` 8]

from32bit n = [chr . fromIntegral $ n .&. 0xff,
               chr . fromIntegral $ (n `shiftR` 8) .&. 0xff,
               chr . fromIntegral $ (n `shiftR` 16) .&. 0xff,
               chr . fromIntegral $ (n `shiftR` 24) .&. 0xff]

hWrite16 :: Handle -> Word16  -> IO ()
hWrite16 handle n = hPutStr handle $ from16bit n


hWrite32 :: Handle -> Word32 -> IO ()
hWrite32 handle n = do hPutStr handle $! from32bit n

writeSamplerate :: Handle -> Int -> IO ()
writeSamplerate handle sr = 
    do cur <- hTell handle
       hSeek    handle AbsoluteSeek 24
       hWrite32 handle (fromIntegral sr)
       hWrite32 handle (fromIntegral sr * 4)
       hSeek    handle AbsoluteSeek cur

writeNumSamples :: Handle -> Int -> IO ()
writeNumSamples handle num =
    do cur <- hTell handle
       let count1 = if even num then num else num-1 --(num * 4)
           count2 = 36 + count1
       hSeek    handle AbsoluteSeek 4
       hWrite32 handle (fromIntegral count2)
       hSeek    handle AbsoluteSeek 40
       hWrite32 handle (fromIntegral count1)
       hSeek    handle AbsoluteSeek cur

writeHeader :: Handle -> IO ()
writeHeader handle = 
    do  write "RIFF"
        write "\xff\xff\xff\xff"
        write "WAVEfmt "
        write32 16
        write16 1
        write16 2                -- Num. channels
        write32 44100            -- Sample rate
        write32 (44100 * 4)  -- Derived from sample rate.
        write16 4
        write16 16
        write "data"
        write "\xff\xff\xff\xff" -- num samples
    where
        write   = do hPutStr handle
        write16 = do hWrite16 handle
        write32 = do hWrite32 handle

writeSamples :: AudioSampleRepr a => Handle -> [a] -> [a] -> IO ()
writeSamples handle ch0 ch1 =
    do let samples = writeS ch0 ch1
       samples `seq` hPutStr handle samples

-- is supposed to be lazy
writeS :: AudioSampleRepr a => [a] -> [a] -> String
writeS [] [] = []
writeS (ch1:chs1) (ch2:chs2)
    = let c1 = toPcmRepr ch1
          c2 = toPcmRepr ch2
      in ((chr . fromIntegral $ c1 .&. 0xff)   :
          (chr . fromIntegral $ c1 `shiftR` 8) :
          (chr . fromIntegral $ c2 .&. 0xff)   :
          (chr . fromIntegral $ c2 `shiftR` 8) :
          writeS chs1 chs2)

{-# SPECIALIZE writeSamples :: Handle -> [Double] -> [Double] -> IO () #-}
{-# SPECIALIZE writeSamples :: Handle -> [Float] -> [Float] -> IO () #-}
{-# INLINE writeSamples #-}
