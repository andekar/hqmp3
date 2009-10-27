{-# OPTIONS -w #-}

module Unpack (unpackMp3) where

-- import Data.Binary.Get 
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
import MP3Types

unpackMp3 :: L.ByteString -> [MP3Header]
unpackMp3 file = runBitGet first file
    where first = do
               skipId3
               init <- lookAhead readHeader
               unpackFrames init

unpackFrames :: (Maybe MP3Header) -> BitGet [(MP3Header)]
unpackFrames Nothing = return [] -- here we want something else!!!
                                 -- like searching for some other valid
                                 -- header
unpackFrames (Just h1) = do
    frame <- readFrameInfo h1
    case frame of
        Left _ -> return [] -- here we want to seek again or maybe quit?!?
        Right (f1,f2) -> do
            frames <- unpackFrames (Just f2) -- MAGIC!
            return $ f1 : frames

-- Finds a header in an mp3 file.
readHeader :: BitGet (Maybe MP3Header)
readHeader = do
    h <- getAsWord16 15
    case h `shiftL` 1 of
        0xFFFA -> do
            prot   <- getBit >>= return . not
            brate' <- getAsWord8 4
            freq'  <- getAsWord8 2
            padd   <- getBit
            skip 1 -- private bit
            mode'  <- getAsWord8 2
            mext'  <- getAsWord8 2
            case getHeaderInfo (brate',freq',mode',mext') of
                Nothing -> return Nothing
                Just ~(brate,freq,mode,mext) -> do
                    skip 4 -- copyright, original & emphasis
                    if prot then skip 16 else return ()
                    sinfo <- readSideInfo mode
                    let size = ((144 * 1000 * brate) `div` freq +
                                (b2i padd)) * 8
                        f' = if prot then 2 else 0
                        ff = case mode of
                                 Mono -> 17
                                 _    -> 32
                        hsize = (f' + ff + 4) * 8
--                         skipp =  ((size - (ff + 4 + f')) * 8)
                    return $ Just (MP3Header brate freq padd mode
                                   mext size hsize sinfo undefined)
                _ -> return Nothing
        _ -> return Nothing
  where
    b2i :: Bool -> Int
    b2i b = if b then 1 else 0

readFrameInfo :: MP3Header -> BitGet (Either (Maybe MP3Header, Bool) MP3Data)
readFrameInfo h1@(MP3Header _ _ _ _ _ fsize hsize sin _) = do
    skipId3
    let pointer = dataPointer sin
    lhs <- getLazyByteString pointer
    -- Need to check the Maybe here
    (s, maybeH2) <- lookAhead $ do
        s <- safeSkip fsize
        if s == fsize then do
            hh <- readHeader
            return (s, hh) else return (s, Nothing)
    case maybeH2 of
        Just h2 ->  do
            let pointer' = (dataPointer . sideInfo) h2
                reads = fsize - pointer'
            -- later we want to parse here
            rhs <- getLazyByteString reads
            --     skip reads
            -- note that we have no error checks yet!!
            return $ Right ((h1{mp3Data = L.append lhs rhs}), h2)
        Nothing -> if s == fsize then do
                     return (Left (Nothing, False))
                     else do 
                         rhs <- getLazyByteString s
                         -- decode
                         return $ Left ((Just h1{mp3Data = L.append lhs rhs}
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
        Mono -> do
            bits <- replicateM 4 getBit
            return $ bits
        _    -> do
            bitsL <- replicateM 4 getBit
            bitsR <- replicateM 4 getBit
            return $ (bitsL ++ bitsR)
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
                            0 else undefined
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

getHeaderInfo :: (Word8,Word8,Word8,Word8) ->
                 Maybe (Int,Int,MP3Mode,(Bool,Bool))
getHeaderInfo (br,fr,md,mx) = do
    brate <- getBitRate br
    freq  <- getFreq fr
    mode  <- getMode md
    case mode of
        JointStereo -> do
            mext  <- getModeExt mx
            return (brate,freq,mode,mext)
        _ -> return (brate,freq,mode,(False,False))
 where
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

     getMode :: Word8 -> Maybe MP3Mode
     getMode w = case w of
         0x00 -> Just Stereo
         0x01 -> Just JointStereo
         0x02 -> Just DualChannel
         0x03 -> Just Mono
         _ -> Nothing

     getModeExt :: Word8 -> Maybe (Bool,Bool)
     getModeExt w = case w of
         0x00 -> Just (False,False)
         0x01 -> Just (False,True)
         0x02 -> Just (True,False)
         0x03 -> Just (True,True)
         _    -> Nothing