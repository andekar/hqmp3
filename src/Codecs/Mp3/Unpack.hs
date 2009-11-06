{-# OPTIONS -w #-}

module Unpack (unpackMp3) where

import BitGet
import qualified Huffman as Huff
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import ID3
import Debug.Trace
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Control.Monad.Identity
import MP3Types
import qualified BitString as BITS

unpackMp3 :: L.ByteString -> [MP3Header]
unpackMp3 file = runBitGet first $ BITS.convert file
    where first = do
               skipId3
               init <- lookAhead $ runMaybeT readHeader
               unpackFrames init

unpackFrames :: (Maybe MP3Header) -> BitGet [MP3Header]
unpackFrames Nothing = undefined -- do
--     r <- getAtLeast 33
--     if r then findHeader >> readHeader >>= unpackFrames
--          else return []
unpackFrames (Just h1) = do
    frame <- readFrameInfo h1
    case frame of
        Left _ -> return [] -- here we want to seek again or maybe quit?!?
        Right (f1,f2) -> do
            frames <- unpackFrames (Just f2) -- MAGIC!
            return $ f1 : frames

-- | findheader will skip until it finds two consecutive sync words
-- the syncword consist of fifteen ones
findHeader :: BitGet ()
findHeader = do r <- getAtLeast 1
                when r $ do
                   skip 1
                   h1 <- lookAhead $ runMaybeT $ readHeader
                   case h1 of
                       Just h1' -> do 
                           h2 <-lookAhead $ do
                               skip $ fromIntegral $ fsize h1'
                               runMaybeT readHeader
                           case h2 of
                               Just h2' -> return ()
                               Nothing  -> findHeader
                       Nothing -> findHeader

syncWord :: BitGet Bool
syncWord = do
    h <- getAsWord16 15
    return (0x7FFF == h)

-- Finds a header in an mp3 file.
readHeader :: MaybeT (BitGetT Identity) MP3Header
readHeader = do
    h <- lift $ getAsWord16 15
    case h `shiftL` 1 of
        0xFFFA -> do
            prot   <- lift $ liftM not getBit
            brate <- getBitRate
            freq  <- getFreq
            padd   <- lift getBit
            lift $ skip 1 -- private bit
            mode  <- getMode
            mext'  <- getModeExt
            lift $ skip 4 -- copyright, original & emphasis
            when prot $ lift $ skip 16
            sinfo <- lift $ readSideInfo mode
            let size = ((144 * 1000 * brate) `div` freq +
                        b2i padd) * 8
                f' = if prot then 2 else 0
                ff = case mode of
                    Mono -> 17
                    _    -> 32
                hsize = (f' + ff + 4) * 8
                mext = case mode of
                    JointStereo -> mext'
                    _ -> (False,False)
            return (MP3Header brate freq padd mode
                    mext size hsize sinfo undefined)
        _ -> fail ""
  where
    b2i :: Bool -> Int
    b2i b = if b then 1 else 0

getBitRate :: MaybeT (BitGetT Identity) Int
getBitRate = do 
    w <- lift $ getAsWord8 4
    case w of
        0x01 ->  return 32
        0x02 ->  return 40
        0x03 ->  return 48
        0x04 ->  return 56
        0x05 ->  return 64
        0x06 ->  return 80
        0x07 ->  return 96
        0x08 ->  return 112
        0x09 ->  return 128
        0x0A ->  return 160
        0x0B ->  return 192
        0x0C ->  return 224
        0x0D ->  return 256
        0x0E ->  return 320
        _ -> fail ""

-- Bit shifting is the most fun I've ever done!
getFreq :: MaybeT (BitGetT Identity) Int
getFreq = do
    w <- lift $ getAsWord8 2
    case w of
        0x00 ->  return 44100
        0x01 ->  return 48000
        0x02 ->  return 32000
        _ -> fail ""

getMode :: MaybeT (BitGetT Identity) MP3Mode
getMode = do
    w <- lift $ getAsWord8 2
    case w of
        0x00 ->  return Stereo
        0x01 ->  return JointStereo
        0x02 ->  return DualChannel
        0x03 ->  return Mono
        _ -> fail ""

getModeExt :: MaybeT (BitGetT Identity) (Bool,Bool)
getModeExt = do
    w <- lift $ getAsWord8 2
    case w of
        0x00 ->  return (False,False)
        0x01 ->  return (False,True)
        0x02 ->  return (True,False)
        0x03 ->  return (True,True)
        _ -> fail ""

readFrameInfo :: MP3Header -> BitGet (Either (Maybe MP3Header, Bool) MP3Data)
readFrameInfo h1@(MP3Header _ _ _ _ _ fsize hsize sin _) = do
    skipId3
    let pointer = dataPointer sin
    lhs <- getBits $ fromIntegral pointer
    -- Need to check the Maybe here
    (s, maybeH2) <- lookAhead $ do
        skip $ fromIntegral fsize -- should be safeSkip when implemented!! to s
        if fsize == fsize then do -- should be s
            hh <- runMaybeT readHeader
            return (fsize, hh) else return (fsize, Nothing) -- should be s
    case maybeH2 of
        Just h2 ->  do
            let pointer' = (dataPointer . sideInfo) h2
                reads = fsize - pointer'
            rhs <- getBits $ fromIntegral reads
            return $ Right ((h1{mp3Data = BITS.append lhs rhs}), h2)
        Nothing -> if s == fsize then return $ Left (Nothing, False)
                     else do 
                         rhs <- getBits $ fromIntegral s
                         return $ Left ((Just h1{mp3Data = BITS.append lhs rhs}
                                        , False))

-- Almost exactly follows the ISO standard
readSideInfo :: MP3Mode -> BitGet SideInfo
readSideInfo mode = do
    dataptr  <- getInt 9
    skipPrivate
    scaleFactors <- getScaleFactors
    chanInfo <- case mode of
        Mono -> do g1 <- getGranule
                   g2 <-getGranule
                   return (Single dataptr scaleFactors g1 g2)
        Stereo -> do g1 <- getGranule
                     g2 <- getGranule
                     g3 <- getGranule
                     g4 <- getGranule
                     return (Dual dataptr scaleFactors g1 g2 g3 g4)
    return chanInfo
  where
    skipPrivate = case mode of
        Mono -> skip 5
        _    -> skip 3
    getScaleFactors = case mode of
        Mono -> replicateM 4 getBit
        _    -> do
            bitsL <- replicateM 4 getBit
            bitsR <- replicateM 4 getBit
            return (bitsL ++ bitsR)
    getGranule = do
            scaleBits        <- getInt 12
            bigValues        <- getInt 9
            globalGain       <- getInt 8
            scaleFacCompress <- getInt 4
            windowSwitching  <- getBit

            blockType  <- getInt $ if windowSwitching then 2 else 0
            mixedBlock <- if windowSwitching then getBit else return False
            tableSelect1  <- getInt 5
            tableSelect2  <- getInt 5
            tableSelect3  <- if windowSwitching then return 0 else getInt 5
            subBlockGain1 <- if windowSwitching then getInt 3 else return 0
            subBlockGain2 <- if windowSwitching then getInt 3 else return 0
            subBlockGain3 <- if windowSwitching then getInt 3 else return 0
            region0Count  <- if windowSwitching then 
                                return (reg0 mixedBlock blockType)
                                else getInt 4
            region1Count  <- if windowSwitching then return reg1 else getInt 3
            -- NOT YET FINISHED
            let reg0len = if not windowSwitching && blockType == 2 then
                          36 else region0Count + 1
                reg1len = if not windowSwitching && blockType == 2 then
                            576 else region0Count + 1 + region1Count + 1
                reg2len = if blockType == 2 then
                            0 else 0 --undefined
--             ifWindow <- if windowSwitching then getWinTrue else getWinFalse
            preFlag       <- getBit
            scaleFacScale <- getBit
            count1TableSelect <- getBit
            return $ Granule scaleBits bigValues globalGain scaleFacCompress
                             windowSwitching blockType mixedBlock tableSelect1
                             tableSelect2 tableSelect3 subBlockGain1
                             subBlockGain2 subBlockGain3
                             preFlag scaleFacScale
                             count1TableSelect reg0len reg1len reg2len
        where
            reg0 False bType | bType == 2 = 8
                             | otherwise = 7
            reg0 _ _ = 7
            reg1 = 36