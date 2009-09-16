-- This module can extract PCM data and frequensies from WAV files.
module Codecs.Wav.Wav ( RiffChunk(..)
           , Chunkdata(..)
           , DataChunk(..)
           , getRiffChunk) where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as Bc
import Data.Word
import Data.Bits

data RiffChunk = RiffChunk { id        :: String
                           , cSize     :: Int
                           , waveId    :: String
                           , chunkData :: [Chunkdata]
                           }
    deriving Show

data Chunkdata = Chunkdata { ckId                :: String
                           , ckSize              :: Int
                           , wFormatTag          :: Int
                           , nChannels           :: Int
                           , nSamplesPerSec      :: Int
                           , nAvgBytesPerSec     :: Int
                           , nBlockAlign         :: Int
                           , wBitsPerSample      :: Int
                           , cbSize              :: Int
                           , wValidBitsPerSample :: Int
                           , dwChannelMask       :: Int
                           , chunkData'          :: DataChunk
                           }
    deriving Show

data DataChunk = DataChunk { dchId   :: String
                           , dchSize :: Int
                           , samples :: B.ByteString -- should be PCM format
                           }
    deriving Show

mains = do src <- B.readFile "song.wav"
           print $ getRiffChunk src
           return ()

getRiffChunk :: B.ByteString -> RiffChunk
getRiffChunk src = let (ids, bs')   = B.splitAt 4 src
                       (size, bs'') = B.splitAt 4 bs'
                       (wId, rest)  = B.splitAt 4 bs''
                   in RiffChunk (B.unpack ids) (calcValue size) (B.unpack wId)
                                [getChunkData rest]

getChunkData :: B.ByteString -> Chunkdata
getChunkData bs = let (cId, bs')   = B.splitAt 4 bs
                      (size, bs'') = B.splitAt 4 bs'
                      (fmt, r)     = B.splitAt 2 bs''
                      (chans, b)   = B.splitAt 2 r
                      (smps, b')   = B.splitAt 4 b
                      (drate, b'') = B.splitAt 4 b'
                      (dblock, bc) = B.splitAt 2 b''
                      (bps, bc')   = B.splitAt 2 bc
                  in Chunkdata (B.unpack cId) (calcValue size)
                               (calcValue fmt) (calcValue chans)
                               (calcValue smps)
                               (calcValue drate) (calcValue dblock)
                               (calcValue bps) 0 0 0 (getDataChunk bc')

getDataChunk :: B.ByteString -> DataChunk
getDataChunk bs = let (cId, bs')    = B.splitAt 4 bs
                      (cSize, bs'') = B.splitAt 4 bs'
                      cSize'        = calcValue cSize
                      (chunk, _) = B.splitAt cSize' bs''
                  in DataChunk (B.unpack cId) cSize' chunk

-- | calculates a value the "INTEL style"
calcValue :: B.ByteString -> Int
calcValue bs = (fromIntegral . foldl1 (.|.))
               (calc 0 (map fromIntegral $ Bc.unpack bs))
    where calc :: Int -> [Word32] -> [Word32]
          calc _ [] = []
          calc num (x:xs) = shiftL x num : calc (num + 8) xs
