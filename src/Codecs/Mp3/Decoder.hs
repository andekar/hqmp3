{-# OPTIONS -w #-}

module Main () where

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
    -- windowzzz
  , blockType       :: Int      -- 2 bits
  , mixedBlock      :: Bool     -- 1 bit
  , tableSelect_1   :: Int      -- 5 bits
  , tableSelect_2   :: Int      -- 5 bits

    -- window == 0
  , tableSelect_3   :: Int

    -- window == 1
  , subBlockGain1   :: Int      -- 3 bits
  , subBlockGain2   :: Int      -- 3 bits
  , subBlockGain3   :: Int      -- 3 bits

  , preFlag           :: Bool       -- 1 bit
  , scaleFacScale     :: Bool       -- 1 bit
  , count1TableSelect :: Bool       -- 1 bit

  -- calculated from precious values
  , region0Start     :: Int
  , region1Start     :: Int
  , region2Start     :: Int
} deriving Show

data AudioData = AudioData

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Bjorn Edstrom
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

type MP3Data = (MP3Header, MP3Header)

type Scales = ([Word8], [[Word8]], Int)
type ChannelData = ([(Int, Int)],Scales)
data DChannel
    = DMono ChannelData ChannelData 
    | DStereo ChannelData ChannelData ChannelData ChannelData

-- Does decoding of granules given the main_data() chunk
-- This function does huffman decoding, and extracts the 
-- scale factor stuff as described in p.18 in ISO-11172-3
decodeGranules :: SideInfo -> L.ByteString -> DChannel
decodeGranules (Single _ scfsi gran1 gran2) bs = 
    let (g1,scale1@(p,_,_),bs') = decodeGranule [] scfsi gran1 bs
        (g2,scale2,_)   = decodeGranule p scfsi gran2 bs'
    in DMono (g1,scale1) (g2,scale2)
decodeGranules (Dual _ scfsi gran1 gran2 gran3 gran4) bs =
    let (g1,scale1@(p,_,_),bs')     = decodeGranule [] scfsi gran1 bs
        (g2,scale2@(p',_,_),bs'')   = decodeGranule p scfsi gran2 bs'
        (g3,scale3@(p'',_,_),bs''') = decodeGranule p' scfsi gran3 bs''
        (g4,scale4,_)     = decodeGranule p'' scfsi gran4 bs'''
    in DStereo (g1,scale1) (g2,scale2) (g3,scale3) (g4,scale4) 

decodeGranule :: [Word8] -> [Bool] -> Granule -> L.ByteString
                  -> ([(Int,Int)], Scales, L.ByteString)
decodeGranule prev scfsi (Granule scaleBits bigValues globalGain
                          scaleFacCompress windowSwitching blockType
                          mixedBlockFlag tableSelect_1 tableSelect_2
                          tableSelect_3 subBlockGain1 subBlockGain2
                          subBlockGain3 preFlag scaleFacScale count1TableSelect
                          region0start region1start region2start) dat
    = flip runBitGet dat $ do wedontcare <- pScaleFactors prev
                              huffData   <- huffDecode undefined undefined undefined
                              (rest,_) <- getRemainingLazy
                              return (huffData, wedontcare, rest)
    where
          (slen1, slen2) = tableScaleLength ! scaleFacCompress
          (scfsi0:scfsi1:scfsi2:scfsi3:[]) = scfsi
          -- will return a list of long scalefactors
          -- a list of lists of short scalefactors
          -- an int describing how much we read, this might not be needed
          pScaleFactors :: [Word8] -> BitGet ([Word8], [[Word8]], Int) 
          pScaleFactors  prev
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
                  s0 <- if c scfsi0 then
                            replicateM 6 $ getAsWord8 slen1
                            else return $ take 6 prev
                  s1 <- if c scfsi1 then
                            replicateM 5 $ getAsWord8 slen1
                            else return $ take 5 $ drop 6 prev
                  -- slen2: 11 to 20
                  s2 <- if c scfsi2 then
                            replicateM 5 $ getAsWord8 slen2
                            else return $ take 6 $ drop 11 prev
                  s3 <- if c scfsi3 then
                            replicateM 5 $ getAsWord8 slen2
                            else return $ take 6 $ drop 16 prev
                  let length = 6 * (if c scfsi0 then slen1 else 0) +
                               5 * (if c scfsi1 then slen1 else 0) +
                               5 * (if c scfsi2 then slen1 else 0) +
                               5 * (if c scfsi3 then slen1 else 0)
                      -- here we might need a paddig 0 after s3
                      -- (if we want a list of 22 elements)
                  return (s0 ++ s1 ++ s2 ++ s3, [], length)
                      where c sc = not sc || (not $ null prev)

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
            --
            let reg0 = if not windowSwitching && blockType == 2 then
                            36 else region0Count + 1
                reg1 = if not windowSwitching && blockType == 2 then
                            576 else region0Count + 1 + region1Count + 1
                reg2 = if blockType == 2 then
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
                             count1TableSelect reg0 reg1 reg2
                where
        reg0 False bType | bType == 2 = 8
                         | otherwise = 7
        reg0 _ _ = 7
        reg1 = 36

type HuffTree = (Huff.HuffTree (Int, Int), Int)

huffDecode :: (Int, Int, Int) -> (HuffTree, HuffTree, HuffTree)
              -> Int -> BitGet [(Int, Int)]
huffDecode (r0, r1, r2) (t0, t1, t2) count1 = do
    r0res <- replicateM r0 $ huffDecodeXY t0
    r1res <- replicateM r1 $ huffDecodeXY t1
    r2res <- replicateM r2 $ huffDecodeXY t2
    return $ r0res ++ r1res ++ r2res

huffDecodeXY :: HuffTree -> BitGet (Int,Int)
huffDecodeXY (huff, linbits) = do
    Just (x,y) <- Huff.decode huff return 0
    x' <-linsign x linbits
    y' <-linsign y linbits
    return (x',y')
  where linsign :: Int -> Int -> BitGet Int
        linsign c l 
            | abs c == 15 && l > 0 = do
                res <- getInt l >>= \r -> return (r+15)
                sign <-getBit
                if sign then return (negate res)
                        else return res
            | c /= 0 = getBit >>= \s -> if s then return (negate c) else return c
            | otherwise = return c

huffDecodeVWXY :: Huff.HuffTree (Int,Int,Int,Int) -> BitGet (Int,Int,Int,Int)
huffDecodeVWXY huff = do
    Just (v,w,x,y) <- Huff.decode huff return 0
    v' <- setSign v
    w' <- setSign w
    x' <- setSign x
    y' <- setSign y
    return (v',w',x',y')
  where setSign 0 = return 0
        setSign c = getBit >>= \s -> if s then return (negate c) else return c

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
