{-# OPTIONS -w #-}
module Main (main, test) where
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
import Control.Monad.ST
import Data.Array.ST

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
        dc = runST $ decodeFrames fs
        f = fs `par` dc `par` "wee"
    putStr f
--     mapM_ print dc
    forM_ dc $ \dchan -> do
        let (left,right) = dchan
--        PCM.writeSamplerate out 44100 -- we need to write this all the time...
        PCM.writeSamples out left right
        cur <- hTell out
        PCM.writeNumSamples out $ fromIntegral $ (cur - 44)
--         putStrLn "# Oh yeah!"
