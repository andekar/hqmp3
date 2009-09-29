module Decoder where

-- in case this is too slow we might change to strict
import Data.Binary.Get 
import Data.Bits
import Data.Word
import qualified Data.ByteString.Lazy as B
import ID3

data MP3Mode = Stereo | JointStereo | DualChannel | Mono deriving Show
data MP3Header = MP3Header {
    bitRate   :: Int
  , frequency :: Int
  , padding   :: Bool
  , mode      :: MP3Mode
  , mext      :: (Bool,Bool) --used only with jointstereo
} deriving Show

test :: FilePath -> IO ()
test file = do
    f <- B.readFile file
    print $ runGet findHeader f

-- Finds a header in an mp3 file.
findHeader :: Get (Maybe MP3Header)
findHeader = do
    skipId3
    h <- getWord16be
    case h .|. 1 of
        0xFFFB -> do
            byte3 <- getWord8
            byte4 <- getWord8
            let brate = getBitRate byte3
                freq  = getFreq byte3
                padd  = isPadded byte3
                mode  = getMode byte4
                mext  = case mode of
                    JointStereo -> getModeExt byte4
                    _           -> (False,False)
            -- TODO fromJust
            return Just $ MP3Header brate freq padd mode mext
        _      -> Nothing

-- This function is so much fun!
getBitRate :: Word8 -> Maybe Int
getBitRate w = case w `shiftR` 4 of
    0x01 -> Just 32
    0x02 -> Just 40
    0x03 -> Just 48
    0x04 -> Just 56
    0x05 -> Just 64
    0x06 -> Just 80
    0x07 -> Just 96
    0x08 -> Just 112
    0x09 -> Just 128
    0x0A -> Just 160
    0x0B -> Just 192
    0x0C -> Just 224
    0x0D -> Just 256
    0x0E -> Just 320
    _    -> Nothing

-- Bit shifting is the most fun I've ever done!
getFreq :: Word8 -> Maybe Int
getFreq w = case w .&. 0x0C `shiftR` 2 of
    0x00 -> Just 44100
    0x01 -> Just 48000
    0x02 -> Just 32000
    _    -> Nothing

isPadded :: Word8 -> Bool
isPadded = not . flip testBit 7

getMode :: Word8 -> MP3Mode
getMode w = case w `shiftR` 6 of
    0x00 -> Stereo
    0x01 -> JointStereo
    0x02 -> DualChannel
    0x03 -> Mono

getModeExt :: Word8 -> (Bool,Bool)
getModeExt w = case w `shiftR` 4 .&. 0x03 of
    0x00 -> (False,False)
    0x01 -> (False,True)
    0x02 -> (True,False)
    0x03 -> (True,True)
