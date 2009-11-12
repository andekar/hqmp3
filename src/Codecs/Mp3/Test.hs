module Test () where
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

main = test "/home/tobsi/machinae_supremacy-cryosleep.mp3"

test :: FilePath -> IO ()
test f = do
    file <- L.readFile f
    let fs = unpackMp3 file
        r = decodeFrame fs
--     mapM_ print fs
    mapM_ (\(f,r) -> print ("Frame: " ++ (show f)) >> print r) (zip [1..] r)