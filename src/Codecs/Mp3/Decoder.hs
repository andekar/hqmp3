{-# OPTIONS -w #-}

module Main () where

-- import Data.Binary.Get 
import BitGet
-- import Data.Binary.Strict.BitGet
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import ID3
import Debug.Trace
import Control.Monad

import Data.Array.Unboxed

data MP3Mode = Stereo | JointStereo | DualChannel | Mono deriving (Show,Eq)
data MP3Header
    = MP3Header { bitRate   :: Int
                , frequency :: Int
                , padding   :: Bool
                , mode      :: MP3Mode
                , mext      :: (Bool,Bool) -- used only with jointstereo
                , fsize     :: Int -- frame  size
                , hsize     :: Int -- header size
                , sideInfo  :: SideInfo
                , mp3Data   :: L.ByteString
} deriving Show

-- Derived from page 17 in ISO-11172-3
-- The side info is totalling 17 or 32 bits, mono and stereo, respectively
data SideInfo
    = Single { dataPointer :: Int -- 9 bits
             , scales  :: [Bool]
             , gran1   :: Granule
             , gran2   :: Granule }
    | Dual   { dataPointer :: Int -- 9 bits
             , scales' :: [Bool]
             , gran1'  :: Granule
             , gran2'  :: Granule
             , gran3'  :: Granule
             , gran4'  :: Granule } deriving Show

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

data AudioData = AudioData

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Björn Edström
tableScaleLength :: Array Int (Int,Int)
tableScaleLength = listArray (0,15)
        [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
         (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)] 

main = test "/home/tobsi/machinae_supremacy-cryosleep.mp3"

test :: FilePath -> IO ()
test f = do
    file <- L.readFile f
    case runBitGet first file of
        ss -> mapM_ print ss
  where 
    readMp3 :: MP3Header -> BitGet [MP3Header]
    readMp3 h1 = do
        frame <- readFrameInfo h1
        case frame of
            Left _ -> return []
            Right (f1,f2) -> do
                frames <- readMp3 f2
                return $ f1 : frames
    first = do
        skipId3
        (Just h1) <- lookAhead readHeader
        readMp3 h1
    rr = undefined

type MP3Data = (MP3Header, MP3Header)

readFrameInfo :: MP3Header -> BitGet (Either (Maybe MP3Header, Bool) MP3Data)
readFrameInfo h1@(MP3Header _ _ _ _ _ fsize hsize sin _) = do
    skipId3
    let pointer = dataPointer sin
    lhs <- getLazyByteString point
    -- Need to check the Maybe here
    (Just h2) <- lookAhead $ skip fsize >> readHeader
    let pointer' = (dataPointer . sideInfo) h2
        reads = fsize - pointer' -- the size of data to read!!! YEAH!
    -- later we want to parse here
    rhs <- getLazyByteString reads
--     skip reads -- will be better later with getBytes!
    -- note that we have no error checks yet!!
    return $ Right ((h1{mp3Data = L.append lhs rhs}), h2)

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
            ifWindow <- if windowSwitching then getWinTrue else getWinFalse
            preFlag       <- getBit
            scaleFacScale <- getBit
            count1TableSelect <- getBit
            return $ Granule scaleBits bigValues globalGain scaleFacCompress
                             windowSwitching ifWindow preFlag scaleFacScale
                             count1TableSelect
                where
        getWinTrue  = do
            blockType     <- getInt 2
            mixedBlock    <- getBit
            tableSelect1  <- getInt 5
            tableSelect2  <- getInt 5
            subBlockGain1 <- getInt 3
            subBlockGain2 <- getInt 3
            subBlockGain3 <- getInt 3
            return $ WT $ WinTrue blockType mixedBlock tableSelect1
                                  tableSelect2 subBlockGain1 subBlockGain2
                                  subBlockGain3
        getWinFalse = do
            tableSelect1 <- getInt 5
            tableSelect2 <- getInt 5
            tableSelect3 <- getInt 5
            region0Count <- getInt 4
            region1Count <- getInt 3
            return $ WF $ WinFalse tableSelect1 tableSelect2 tableSelect3
                                   region0Count region1Count

decodeMainData :: [(MP3Header, L.ByteString)] -> [AudioData]
decodeMainData = undefined

getMainData :: MP3Header -> BitGet (Maybe AudioData)
getMainData (MP3Header bitRate frequency padding mode mext fsize hsize
             sideInfo r) = undefined
    where mainData'  = replicateM (2 * mode') mainData''
          mainData'' :: BitGet AudioData
          mainData'' | True = undefined
          mode' | mode == Mono = 1
                | otherwise = 2
          -- will return a list of long scalefactors
          -- a list of lists of short scalefactors
          -- an int describing how much we read, this might not be needed
          pScaleFactors :: Int -> Bool -> Int -> Int -> [Bool] -> Int -> [Word8] ->
                    BitGet ([Word8], [[Word8]], Int) 
          pScaleFactors blockType mixedBlockFlag slen1 slen2
                 (scfsi0:scfsi1:scfsi2:scfsi3:[]) gr prev
                  -- as defined in page 18 of the mp3 iso standard
              | blockType == 2 && mixedBlockFlag = do
                  -- slen1: 0 to 7 (long window scalefactor band)
                  scalefacL0 <- replicateM 8 $ getAsWord8 slen1
                  -- slen1: bands 3 to 5 (short window scalefactor band)
                  scaleFacS0 <- replicateM 3 $ replicateM 3 $ getAsWord8 slen1
                  -- slen2: bands 6 to 11
                  scaleFacS1 <- replicateM 6 $ replicateM 3 $ getAsWord8 slen2
                  let length = 17 * slen1 + 18 * slen2
                  -- here we must insert 3 lists of all zeroes since the
                  -- slen1 starts at band 3
                      sr = replicate 3 $ replicate 3 0
                  return (scalefacL0, sr ++ scaleFacS0 ++ scaleFacS1, length)
              | blockType == 2 = do
                  -- slen1: 0 to 5
                  scaleFacS0 <- replicateM 6 $ replicateM 3 $ getAsWord8 slen1
                  -- slen2: 6 to 11
                  scaleFacS1 <- replicateM 6 $ replicateM 3 $ getAsWord8 slen2
                  let length = 18 * slen1 + 18 * slen2
                  return ([], scaleFacS0 ++ scaleFacS1, length)
              | otherwise = do
                  -- slen1: 0 to 10
                  s0 <- if c scfsi0 gr then
                            replicateM 6 $ getAsWord8 slen1
                            else return $ take 6 prev
                  s1 <- if c scfsi1 gr then
                            replicateM 5 $ getAsWord8 slen1
                            else return $ take 5 $ drop 6 prev
                  -- slen2: 11 to 20
                  s2 <- if c scfsi2 gr then
                            replicateM 5 $ getAsWord8 slen2
                            else return $ take 6 $ drop 11 prev
                  s3 <- if c scfsi3 gr then
                            replicateM 5 $ getAsWord8 slen2
                            else return $ take 6 $ drop 16 prev
                  let length = 6 * (if c scfsi0 gr then slen1 else 0) +
                               5 * (if c scfsi1 gr then slen1 else 0) +
                               5 * (if c scfsi2 gr then slen1 else 0) +
                               5 * (if c scfsi3 gr then slen1 else 0)
                      -- here we might need a paddig 0 after s3
                      -- (if we want a list of 22 elements)
                  return (s0 ++ s1 ++ s2 ++ s3, [], length)
                      where c sc gr = not sc || gr == 0

getInt :: Int -> BitGet Int
getInt i
    | i <= 8    = getAsWord8 i  >>= return . fromIntegral
    | i <= 16   = getAsWord16 i >>= return . fromIntegral
--    | i <= 32   = getAsWord32 i >>= return . fromIntegral
    | otherwise = error "64 bits does not fit into an Int"

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
