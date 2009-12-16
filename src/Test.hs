{-# OPTIONS -w #-}
module Main (test) where
import Data.Binary.BitString.BitGet
-- import qualified Codec.Compression.Huffman.Huffman as Huff
-- import Data.Binary.Strict.BitGet
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Codecs.Mp3.ID3
import Debug.Trace
import Control.Monad
import Control.Parallel
import Codecs.Mp3.Decoder
import Codecs.Mp3.MP3Types
import Codecs.Mp3.Unpack
import System.Environment

import qualified PCMWriter as PCM
import System.IO

main = do
    (arg:[]) <- getArgs
    test arg

test :: FilePath -> IO ()
test f = do
    file <- L.readFile f
    out  <- openBinaryFile "out.wav" WriteMode
    PCM.writeHeader out
    let fs = unpackMp3 file
        dc = decodeFrames fs
        f = fs `par` dc `par` "wee"
    putStr f
--     mapM_ print dc
    forM_ dc $ \dchan -> do
        let (left,right) = dchan
        PCM.writeSamplerate out 44100 -- we need to write this all the time...
        PCM.writeSamples out left right
--         putStrLn "# Oh yeah!"
  where
--       extract :: ([Double], [Double]) -> (Double)
    extract :: DChannel [Double] -> ([Double],[Double])
    extract chan = case chan of
        Single _ _ _ (g0, g1) -> (fromChanData $ mp3Data g0, fromChanData $ mp3Data g1)
        Dual _ _ _ (g0, g2) (g1, g3) -> 
            ( (fromChanData $ mp3Data g0) ++ (fromChanData $ mp3Data g2), 
              (fromChanData $ mp3Data g1) ++ (fromChanData $ mp3Data g3) )

    fromChanData :: ChannelData [Double] -> [Double]
    fromChanData (ChannelData _ as) = as
