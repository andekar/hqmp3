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

main = test "song4.mp3"

test :: FilePath -> IO ()
test f = do
    file <- L.readFile f
    let fs = unpackMp3 file
        r = decodeFrame fs
--     mapM_ print fs
    mapM_ (\(f,r) -> print ("Frame: " ++ (show f)) >> print (fun r)) (zip [1..] r)

fun :: DChannel a -> Int
fun m = case m of
    (DMono   (g1,ch1) (g2,ch2) ) -> r ch1 + r ch2
    (DStereo (g1,ch1) (g2,ch2) (g3,ch3) (g4,ch4)) -> r ch1 + r ch2 + r ch3 + r ch4
  where r (ChannelData sc list) = length list `seq` 1
        r' (Scales l1 l2) = length l2 `seq` 1