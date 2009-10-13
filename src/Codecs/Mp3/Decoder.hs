{-# OPTIONS -w #-}

module Decoder () where

-- import Data.Binary.Get 
import BitGet
-- import Data.Binary.Strict.BitGet
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import ID3
import Debug.Trace
import Control.Monad

import Data.Array.Unboxed

data MP3Mode = Stereo | JointStereo | DualChannel | Mono deriving (Show,Eq)
data MP3Header = MP3Header {
    bitRate   :: Int
  , frequency :: Int
  , padding   :: Bool
  , mode      :: MP3Mode
  , mext      :: (Bool,Bool) -- used only with jointstereo
  , size      :: Int
  , sideInfo  :: SideInfo
} deriving Show

-- Derived from page 17 in ISO-11172-3
-- The side info is totalling 17 or 32 bits, mono and stereo, respectively
data SideInfo = SideInfo {
    dataPointer     :: Int                      -- 9   bits
--, private_bits    :: Int                      -- 3/5 bits
    -- 4 per channel, [] = no such channel
  , scaleFactor     :: ([Bool],[Bool])          -- 4 bits / channel
    -- two per channel, [] = no such channel
  , granules        :: ([Granule],[Granule])    -- 2 per channel
} deriving Show

-- Granule is computed for each specific channel
data Granule = Granule {
    scaleBits         :: Int        -- 12 bits
  , bigValues         :: Int        -- 9 bits
  , globalGain        :: Int        -- 8 bits
  , scaleFacCompress  :: Int        -- 4 bits scaleLength?
  , windowSwitching   :: Bool       -- 1 bit
  , ifWindow          :: Window
  , preFlag           :: Bool       -- 1 bit
  , scaleFacScale     :: Bool       -- 1 bit
  , count1TableSelect :: Bool       -- 1 bit
} deriving Show

data Window  = WT WinTrue | WF WinFalse deriving Show
data WinTrue = WinTrue {
    blockType       :: Int      -- 2 bits
  , mixedBlock      :: Bool     -- 1 bit
  , tableSelect_1   :: Int      -- 5 bits
  , tableSelect_2   :: Int      -- 5 bits
  , subBlockGain1   :: Int      -- 3 bits
  , subBlockGain2   :: Int      -- 3 bits
  , subBlockGain3   :: Int      -- 3 bits
} deriving Show
data WinFalse = WinFalse {
    tableSelect1    :: Int      -- 5 bits
  , tableSelect2    :: Int      -- 5 bits
  , tableSelect3    :: Int      -- 5 bits
  , region0Count    :: Int      -- 4 bits
  , region1Count    :: Int      -- 3 bits
} deriving Show

data MP3Data = MP3Data

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Björn Edström
tableScaleLength :: Array Int (Int,Int)
tableScaleLength = listArray (0,15)
        [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
         (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)] 

test :: FilePath -> IO ()
test f = do
    file <- B.readFile f
    print $ runBitGet readFrameInfo file

-- test for the BitGet
test' = do
    let bs = B.pack [0xFD::Word8,0xFF::Word8,0x00::Word8]
    print $ runBitGet readStuff bs
        where readStuff = do res  <- getAsWord8 5
                             res' <- getAsWord8 8
                             res'' <- getAsWord8 3
                             return ((res,res', res''))-- && res' == 0x5)

-- Finds a header in an mp3 file.
readFrameInfo :: BitGet (Maybe MP3Header)
readFrameInfo = do
    skipId3 -- is this the right place to do this? YES?!
    h <- getAsWord16 15
    case h `shiftL` 1 of
        0xFFFA -> do
            prot  <- getBit >>= return . not
            brate <- getAsWord8 4 >>= return . getBitRate
            freq  <- getAsWord8 2 >>= return . getFreq
            padd  <- getBit
            skip 1 -- private bit
            mode  <- getAsWord8 2 >>= return . getMode
            mext  <- getAsWord8 2 >>= \b -> case mode of
                JointStereo -> return $ getModeExt b
                _           -> return $ (False,False)
            skip 4 -- copyright, original & emphasis
            if prot then skip 16 else return ()
            case (brate, freq) of
                (Just b, Just f) -> do
                    sinfo <- readSideInfo mode
                    let size = (144 * 1000 * b) `div` f + (b2i padd)
                        f' = if prot then 2 else 0
                        ff = case mode of
                                 Mono -> 17
                                 _    -> 32
                    -- left <- remaining
                    skip ((size - (ff + 4 + f')) * 8) -- delete when we read
                    -- Here we should read all frame data, starting at
                    -- index pointed out by main_data_pointer, TODO
                    trace ("read frame!") readFrameInfo
                _   -> trace "bitrate wrong" return Nothing
        _   -> trace ("bad sync " ++ show (h `shiftL` 1)) return Nothing
  where
    b2i :: Bool -> Int
    b2i b = if b then 1 else 0

-- Does NOT read the "d1" part of a "mixed frame"
getNextHeader :: MP3Header -> BitGet (Maybe MP3Header)
getNextHeader h = lookAhead $ do
    let s = size h
    skip $ s*8
    readFrameInfo

{-
-- Reads the portion of data that is before its header, if any
readD1 :: Int -> BitGet B.ByteString
readD1 0 = return $ B.empty
readD1 x = getLeftByteString (x*8)

-- Reads the main_data()
-- The Int tells us how much to read
readMainData :: MP3Header -> B.ByteString -> Int -> BitGet MP3Data
readMainData header d1 end = do
    d2 <- getLeftByteString (end*8)
    let d = B.append d1 d2
    return undefined
-}

-- Almost exactly follows the ISO standard
readSideInfo :: MP3Mode -> BitGet SideInfo
readSideInfo mode = do
    dataptr  <- getInt 9
    skipPrivate
    scaleFactors <- getScaleFactors
    granuleL     <- getGranule
    granuleR     <- getGranule
    return $ SideInfo dataptr scaleFactors (granuleL, granuleR)
  where
    skipPrivate = case mode of
        Mono -> skip 5
        _    -> skip 3
    getScaleFactors = case mode of
        Mono -> do
            bits <- replicateM 4 getBit
            return (bits, []) 
        _    -> do
            bitsL <- replicateM 4 getBit
            bitsR <- replicateM 4 getBit
            return (bitsL, bitsR)
    getGranule = case mode of
        Mono -> getGranule' >>= \g -> return [g]
        _    -> do
            l <- getGranule'
            r <- getGranule'
            return [l,r]
      where
        getGranule' = do
            scaleBits        <- getInt 12 
            bigValues        <- getInt 9
            globalGain       <- getInt 8
            scaleFacCompress <- getInt 4
            windowSwitching  <- getBit
            ifWindow <- if windowSwitching then getWinTrue else getWinFalse
            preFlag       <- getBit
            scaleFacScale <- getBit
            count1TableSelect <- getBit
            return $ Granule scaleBits bigValues globalGain scaleFacCompress 
                             windowSwitching ifWindow preFlag scaleFacScale
                             count1TableSelect
        getWinTrue  = do
            blockType     <- getInt 2
            mixedBlock    <- getBit
            tableSelect1  <- getInt 5
            tableSelect2  <- getInt 5
            subBlockGain1 <- getInt 3
            subBlockGain2 <- getInt 3
            subBlockGain3 <- getInt 3
            return $ WT $ WinTrue blockType mixedBlock tableSelect1 tableSelect2
                                  subBlockGain1 subBlockGain2 subBlockGain3
        getWinFalse = do
            tableSelect1 <- getInt 5
            tableSelect2 <- getInt 5
            tableSelect3 <- getInt 5
            region0Count <- getInt 4
            region1Count <- getInt 3
            return $ WF $ WinFalse tableSelect1 tableSelect2 tableSelect3
                                   region0Count region1Count

getInt :: Int -> BitGet Int
getInt i
    | i <= 8    = getAsWord8 i  >>= return . fromIntegral
    | i <= 16   = getAsWord16 i >>= return . fromIntegral
--    | i <= 32   = getAsWord32 i >>= return . fromIntegral
    | otherwise = error "64 bits does not fit into an Int"

-- This function is so much fun!
getBitRate :: Word8 -> Maybe Int
getBitRate w = case w of
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
getFreq w = case w of
    0x00 -> Just 44100
    0x01 -> Just 48000
    0x02 -> Just 32000
    _    -> Nothing

getMode :: Word8 -> MP3Mode
getMode w = case w of
    0x00 -> Stereo
    0x01 -> JointStereo
    0x02 -> DualChannel
    0x03 -> Mono
    _ -> error "This should not happen!!! (getMode)"

getModeExt :: Word8 -> (Bool,Bool)
getModeExt w = case w of
    0x00 -> (False,False)
    0x01 -> (False,True)
    0x02 -> (True,False)
    0x03 -> (True,True)
    _ -> error "This should not happen!!! (getModeExt)"
