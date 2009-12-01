{-# OPTIONS -w #-}
module Main (test) where
import BitGet
import qualified Huffman as Huff
-- import Data.Binary.Strict.BitGet
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import ID3
import Debug.Trace
import Control.Monad
import Decoder
import MP3Types
import Unpack

import qualified PCMWriter as PCM
import System.IO

main = test "/home/tobsi/song2.mp3"

test :: FilePath -> IO ()
test f = do
    file <- L.readFile f
    out  <- openBinaryFile "out.wav" WriteMode
    PCM.writeHeader out
    let fs = unpackMp3 file
        dc = decodeFrames fs
    forM_ dc $ \dchan -> do
        let (left,right) = extract dchan
        PCM.writeSamples out left right
        putStrLn "# Oh yeah!"
  where
    extract :: DChannel Double -> ([Double],[Double])
    extract (DMono (_,cd1) (_,cd2))                   = undefined
    extract (DStereo (_,cd1) (_,cd2) (_,cd3) (_,cd4)) = 
        (fromChanData cd1 ++ fromChanData cd3,
         fromChanData cd2 ++ fromChanData cd4)

    fromChanData :: ChannelData Double -> [Double]
    fromChanData (ChannelData _ as) = as

getLast :: DChannel Double -> Double
getLast (DMono _ (_,cd2)) = case cd2 of
    ChannelData _ as -> if not (null as) then last as else 0.0
getLast (DStereo _ _ _ (_,cd4)) = case cd4 of
    ChannelData _ as -> if not (null as) then last as else 0.0

fun :: DChannel a -> Int
fun m = case m of
    (DMono   (g1,ch1) (g2,ch2) ) -> r ch1 + r ch2
    (DStereo (g1,ch1) (g2,ch2) (g3,ch3) (g4,ch4)) -> r ch1 + r ch2 + 
                                                     r ch3 + r ch4
  where r (ChannelData sc list) = length list `seq` 1
        r' (Scales l1 l2) = length l2 `seq` 1