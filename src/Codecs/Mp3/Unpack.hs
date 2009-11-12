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
import Data.Array

unpackMp3 :: L.ByteString -> [MP3Header]
unpackMp3 file = runBitGet first $ BITS.convert file
    where first = do
               skipId3
               init <- lookAhead $ runMaybeT readHeader
               unpackFrames init

unpackFrames :: Maybe EMP3Header -> BitGet [MP3Header]
unpackFrames Nothing = return [] -- do
--     r <- getAtLeast 33
--     if r then findHeader >> readHeader >>= unpackFrames
--          else return []
unpackFrames (Just h1) = do
    frame <- readFrameData h1
    case frame of
        Left _ -> return [] -- here we want to seek again or maybe quit?!?
        Right (f1,f2) -> do
            frames <- unpackFrames (Just f2) -- MAGIC!
            return $ f1 : frames

-- | findheader will skip until it finds two consecutive sync words
-- the syncword consist of fifteen ones
findHeader :: BitGet (Int, Maybe EMP3Header)
findHeader = do
    r <- getAtLeast 1
    if not r then return (0, Nothing) else do
        skip 1
        h1 <- lookAhead $ runMaybeT $ readHeader
        case h1 of
            Just h1' -> do
                h2 <-lookAhead $ do
                    skip $ fromIntegral $ fsize h1'
                    runMaybeT readHeader
                case h2 of
                    Just _ -> return (1, h1)
                    Nothing  -> do 
                        (i',h) <- findHeader
                        return (i' + 1, h)
            Nothing -> do (i,h) <- findHeader
                          return (i + 1, h)

syncWord :: BitGet Bool
syncWord = do
    h <- getAsWord16 15
    return (0x7FFD == h)

-- Finds a header in an mp3 file.
readHeader :: MaybeT BitGet EMP3Header
readHeader = do
    h <- lift $ syncWord
    if not h then fail "" else do
        prot  <- lift $ liftM not getBit
        brate <- getBitRate
        freq  <- getFreq
        padd  <- lift getBit
        lift $ skip 1 -- private bit
        mode  <- getMode
        mext' <- getModeExt
        lift $ skip 4 -- copyright, original & emphasis
        when prot $ lift $ skip 16
        sinfo <- lift $ readSideInfo mode freq
        let size = ((144 * 1000 * brate) `div` freq +
                    if padd then 1 else 0) * 8
            f' = if prot then 2 else 0
            ff = case mode of
                Mono -> 17
                _    -> 32
            hsize = (f' + ff + 4) * 8
            mext = case mode of
                JointStereo -> mext'
                _ -> (False, False)
        return (MP3Header brate freq padd mode
                mext size hsize sinfo ())

getBitRate :: MaybeT BitGet Int
getBitRate = do 
    w <- lift $ getAsWord8 4
    if w == 0 && w == 15 then fail "Bad bitrate"
                         else return $ barr ! w
  where
    barr = listArray (1,14) [32,40,48,56,64,80,96,112,128,160,192,224,256,320]

-- Bit shifting is the most fun I've ever done!
getFreq :: MaybeT BitGet Int
getFreq = do
    w <- lift $ getAsWord8 2
    if w == 0x03 then fail "Bad frequency"
                 else return $ [44100,48000,32000] !! (fromIntegral w)

getMode :: MaybeT BitGet MP3Mode
getMode = do
    w <- lift $ getAsWord8 2
    return $ [Stereo,JointStereo,DualChannel,Mono] !! (fromIntegral w)

getModeExt :: MaybeT BitGet (Bool, Bool)
getModeExt = do
    w <- lift $ getAsWord8 2
    return $ table !! (fromIntegral w)
  where table = [(False, False), (False, True), (True, False), (True, True)]



-- readFrameData :: EMP3Header -> StateT MP3Header BitGet (Either EMP3Header MP3Data)
readFrameData h1@(MP3Header _ _ _ _ _ fsize hsize sin _) = do
    skipId3
    let pointer = dataPointer sin
    lhs <- getBits $ fromIntegral pointer
    (s, maybeH2) <- lookAhead $ do
        s <- safeSkip $ fromIntegral fsize
        if s == BOF then do
            hh <- runMaybeT readHeader
            return (s, hh) else return (s, Nothing)
    case maybeH2 of
        Just h2 ->  do
            let pointer' = (dataPointer . sideInfo) h2
                reads = fsize - pointer'
            rhs <- getBits $ fromIntegral reads
            return $ Right ((h1{mp3Data = BITS.append lhs rhs}), h2)
        Nothing -> if s == BOF then do
                     (i,h2) <- lookAhead findHeader
                     case h2 of
                         Nothing -> return $ Left (Nothing, EOF)
                         Just h2' -> do skip (fromIntegral $ 
                                              (fromIntegral i) - 
                                              (dataPointer $ sideInfo h2'))
                                        readFrameData h2'
                     else do
                         rhs <- getBits $ fromIntegral fsize -- take as much
                         return $ Left ((Just h1{mp3Data = BITS.append lhs rhs}
                                        , s))

-- Almost exactly follows the ISO standard
readSideInfo :: MP3Mode -> Int -> BitGet SideInfo
readSideInfo mode freq = do
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
            bigValues        <- trace ("part23 " ++ show scaleBits) getInt 9
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
            region1Count  <- if windowSwitching then return 0 else getInt 3
            --from this point we have some similar code to the one bjorn uses
            let r0count = if windowSwitching
                             then (if blockType == 2 then 8 else 7)
                             else region0Count
                r1count = if windowSwitching then 20 - r0count
                             else region1Count
                sbTable   = tableScaleBandBoundary freq
                r1bound   = sbTable $ r0count + 1
                r2bound   = sbTable $ r0count + 1 + r1count + 1
                bv2       = bigValues * 2
                reg0len   = trace ("\nbv2: "++ show bv2) $ if blockType == 2 
                                 then min bv2 36
                                 else min bv2 r1bound
                reg1len     = if blockType == 2 
                                 then min (bv2-reg0len) 540 
                                 else min (bv2-reg0len) (r2bound - reg0len) 
                reg2len     = if blockType == 2 
                                 then 0   
                                 else bv2 - (reg0len + reg1len)
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
            reg0 False 2 = 8
            reg0 _ _     = 7

-- some copied stuff from Bjorn!!
--
-- tableScaleBandBound{Long,Short}, tableScaleBandBoundary
--
-- These few tables represent the boundaries, in the 576 frequency 
-- regions, of the scale factor bands. These bands approximate the
-- critical bands of the human auditory system, and are used to
-- determine scaling. This scaling controls the quantization noise.
--
tableScaleBandBoundLong :: Int -> [Int]
tableScaleBandBoundLong 44100 = [  0,   4,   8,  12,  16,  20,  24,  30,
                                  36,  44,  52,  62,  74,  90, 110, 134, 
                                 162, 196, 238, 288, 342, 418, 576] 
tableScaleBandBoundLong 48000 = [  0,   4,   8,  12,  16,  20,  24,  30, 
                                  36,  42,  50,  60,  72,  88, 106, 128, 
                                 156, 190, 230, 276, 330, 384, 576] 
tableScaleBandBoundLong 32000 = [  0,   4,   8,  12,  16,  20,  24,  30,
                                  36,  44,  54,  66,  82, 102, 126, 156, 
                                 194, 240, 296, 364, 448, 550, 576]
tableScaleBandBoundLong _     = error "Wrong SR for Table."


tableScaleBandBoundShort :: Int -> [Int]
tableScaleBandBoundShort 44100 = [  0,   4,   8,  12,  16,  22,  30,  40, 
                                   52,  66,  84, 106, 136, 192] 
tableScaleBandBoundShort 48000 = [  0,   4,   8,  12,  16,  22,  28,  38, 
                                   50,  64,  80, 100, 126, 192] 
tableScaleBandBoundShort 32000 = [  0,   4,   8,  12,  16,  22,  30,  42, 
                                   58,  78, 104, 138, 180, 192]
tableScaleBandBoundShort _     = error "Wrong SR for Table."

-- We only need to export the long boundaries for unpacking (Unpack.hs).
tableScaleBandBoundary :: Int -> Int -> Int
tableScaleBandBoundary sfreq index = (tableScaleBandBoundLong sfreq) !! index