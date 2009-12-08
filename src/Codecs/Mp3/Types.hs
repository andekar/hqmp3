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

module Codecs.Mp3.Types where

-- 
-- Simple types.
--
type Sample = Double
type Frequency = Double

type SampleRate = Int

data ChannelMode = Mono 
                 | Stereo 
                 | DualChannel 
                 | JointStereo 
                 deriving (Eq)

instance Show ChannelMode where
    show Mono        = "Mono"
    show Stereo      = "Stereo"
    show DualChannel = "DualChannel"
    show JointStereo = "JointStereo"

data BlockFlag = LongBlocks 
               | ShortBlocks 
               | MixedBlocks 
               deriving (Eq)

instance Show BlockFlag where
    show LongBlocks  = "LongBlocks"
    show ShortBlocks = "ShortBlocks"
    show MixedBlocks = "MixedBlocks"

-- 
-- For decoding.
--
data MP3FrameHeader = MP3FrameHeader {
    headBitrate    :: Int,
    headSampleRate :: SampleRate,
    headChannels   :: ChannelMode,
    -- details
    headCRC        :: Bool,
    headPadding    :: Int,
    headStereoMS   :: Bool,
    headStereoIS   :: Bool
} deriving (Show)

-- Layer between MP3 and MP3Unpack
data MP3Data = MP3Data1Channels SampleRate ChannelMode (Bool, Bool) 
                                MP3DataChunk MP3DataChunk
             | MP3Data2Channels SampleRate ChannelMode (Bool, Bool) 
                                MP3DataChunk MP3DataChunk 
                                MP3DataChunk MP3DataChunk
             deriving (Show)

mp3dataChunk00 (MP3Data1Channels _ _ _ chunk _)     = chunk
mp3dataChunk00 (MP3Data2Channels _ _ _ chunk _ _ _) = chunk

mp3dataSampleRate (MP3Data1Channels sr _ _ _ _)     = sr
mp3dataSampleRate (MP3Data2Channels sr _ _ _ _ _ _) = sr

data MP3DataChunk = MP3DataChunk {
    chunkBlockType    :: !Int,
    chunkBlockFlag    :: !BlockFlag,
    chunkScaleGain    :: !Double,
    chunkScaleSubGain :: !(Double, Double, Double),
    chunkScaleLong    :: ![Double],
    chunkScaleShort   :: ![[Double]],
    chunkISParam      :: !([Int], [[Int]]),
    chunkData         :: ![Int]
} deriving (Show)


-- Pretty print.

prettyData (MP3Data1Channels sr ch (ms, is) c00 c10) =
        show sr ++ "Hz" ++
        " " ++ printCh ch (ms, is) ++ 
        " Chunks: [" ++ printDC c00 ++ 
        "," ++ printDC c10 ++ "]"

prettyData (MP3Data2Channels sr ch (ms, is) c00 c01 c10 c11) = 
        show sr ++ "Hz" ++
        " " ++ printCh ch (ms, is) ++ 
        " Chunks: [" ++ printDC c00 ++ 
        "," ++ printDC c01 ++
        "," ++ printDC c10 ++
        "," ++ printDC c11 ++ "]"

printDC (MP3DataChunk bt bf _ _ _ _ _ _) = printBF bf ++ "(" ++ show bt ++ ")"

printBF LongBlocks  = "L"
printBF ShortBlocks = "S"
printBF MixedBlocks = "M"

printCh JointStereo (True, True)  = "JointStereo (MS, IS)"
printCh JointStereo (True, False) = "JointStereo (MS)"
printCh JointStereo (False, True) = "JointStereo (IS)"
printCh JointStereo _             = "JointStereo"
printCh ch _                      = show ch


