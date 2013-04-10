{-# OPTIONS -w #-}

module Codecs.Mp3.Unpack where

import ID3

-- Monads and such
import Control.Monad
import Control.Monad.Maybe
import Control.Monad.Trans
import Data.Binary.BitString.BitGet
-- Bit-/ByteStrings
import qualified Data.Binary.BitString.BitString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
-- Other data structures
import Data.Array

type MP3Data a    = (MP3Header a, MP3Header a)
data MP3Mode    = Stereo | JointStereo | DualChannel | Mono deriving (Show,Eq)

data MP3Header a
    = MP3Header { mode      :: !MP3Mode
                , mext      :: !(Bool,Bool) -- used only with jointstereo
                , fsize     :: !Int -- frame  size
                , hsize     :: !Int -- header size
                , sideInfo  :: SideInfo a
                }
    deriving Show

-- Derived from page 17 in ISO-11172-3
-- The side info is totalling 17 or 32 bits, mono and stereo, respectively
data SideInfo a
    = Single { sampleRate  :: !Int
             , dataPointer :: !Int -- 9 bits
             , scales      :: ![Bool]
             , gran        :: !(Granule a, Granule a)
             }
    | Dual   { sampleRate  :: !Int
             , dataPointer :: !Int -- 9 bits
             , scales' :: ![Bool]
             , gran0  :: !(Granule a, Granule a) -- this is g0 and g2
             , gran1  :: !(Granule a, Granule a) -- this is g1 and g3
             } deriving Show

--instance NFData (SideInfo a) where
--     rnf info = info `seq` ()
    

-- Granule is computed for each specific channel
data Granule a = Granule {
--     scaleBits         :: Int        -- 12 bits
    bigValues         :: !Int        -- 9 bits
  , globalGain        :: !Double     -- 8 bits
  , scaleFacCompress  :: !Int        -- 4 bits scaleLength?
  , windowSwitching   :: !Bool       -- 1 bit
    -- windowzzz
  , blockType       :: !Int      -- 2 bits
  , mixedBlock      :: !Bool     -- 1 bit
  , tableSelect_1   :: !Int      -- 5 bits
  , tableSelect_2   :: !Int      -- 5 bits

    -- window == 0
  , tableSelect_3   :: !Int

    -- window == 1
  , subBlockGain1   :: !Double   -- 3 bits
  , subBlockGain2   :: !Double   -- 3 bits
  , subBlockGain3   :: !Double   -- 3 bits

  , preFlag           :: !Bool       -- 1 bit
  , scaleFacScale     :: !Bool       -- 1 bit
  , count1TableSelect :: !Bool       -- 1 bit

  -- calculated from previous values
  , region0Start     :: !Int
  , region1Start     :: !Int
  , region2Start     :: !Int

  , mp3Data          :: a
} deriving Show

instance Functor Granule where
    fmap f gran = gran {mp3Data = f (mp3Data gran)}

--
-- Stuff for reading the headers of mp3 frames
--
data MP3H a b = Error | Corr (MP3Header a, MP3Header b) | Last (MP3Header a)

unpackMp3 :: L.ByteString -> [SideInfo BS.BitString]
unpackMp3 = runBitGet first . BS.convert
    where first = do
               skipId3
               init <- lookAhead $ runMaybeT unpackHeader
               unpackFrames init

-- replace with a statemonad maybe?
-- | unpackFrames use a previous header to locate the new header
-- this is the only way to do it in a fast manner, if
-- we do not find a header, something is broken and we apply a
-- linear search for a new header when we find one we make sure
-- that we have two consective headers so that we are sure this is
-- actually a header.
unpackFrames :: Maybe (MP3Header Int) -> BitGet [SideInfo BS.BitString]
unpackFrames Nothing = do
    r <- getAtLeast 33
    head <- findHeader
    if r then unpackFrames $ snd head
         else return []
unpackFrames (Just h1) = do
    frame <- unpackMainData h1
    case frame of
        Error -> return [] -- usually we have already tried to fast forward
        Last  h -> return [sideInfo h]
        Corr (f1,f2) -> do
            frames <- unpackFrames $ Just f2
            return $ (sideInfo f1) : frames

-- | The syncword for a header is simply 15 consecutive
-- ones.
syncWord :: BitGet Bool
syncWord = do
    h <- getAsWord16 15
    return (0x7FFD == h)

-- | findheader will skip until it finds two consecutive sync words
-- the syncword consist of fifteen ones
findHeader :: BitGet (Int, Maybe (MP3Header Int))
findHeader = do
    r <- getAtLeast 1
    if not r then return (0, Nothing) else do
        skip 1
        h1 <- lookAhead $ runMaybeT unpackHeader
        case h1 of
            Just h1' -> do
                h2 <- lookAhead $ do
                    skip $ fi $ fsize h1'
                    runMaybeT unpackHeader
                case h2 of
                    Just _ -> return (1, h1)
                    Nothing  -> do 
                        (i',h) <- findHeader
                        return (i' + 1, h)
		-- We should be smarter here, not only skip one bit
            Nothing -> do (i,h) <- findHeader
                          return (i + 1, h)


-- Finds and parses a header in an mp3 file.
unpackHeader :: MaybeT BitGet (MP3Header Int)
unpackHeader = do
    h <- lift syncWord
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
        sinfo <- lift $ unpackSideInfo mode freq
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
        return $ MP3Header mode mext size hsize sinfo

-- | Four bits decide the bitrate of the file,
-- this can be used to calculate where the next frame is located.
-- This value is not used when doing the decoding of the mp3 file.
getBitRate :: MaybeT BitGet Int
getBitRate = do
    w <- lift $ getAsWord8 4
    if w == 0 || w == 15 then fail "Bad bitrate"
                         else return $ barr ! w
  where
    barr = listArray (1,14) [32,40,48,56,64,80,96,112,128,160,192,224,256,320]

-- | The frequency is used when calculating the next header
-- also it is used in the decoding phase.
getFreq :: MaybeT BitGet Int
getFreq = do
    w <- lift $ getAsWord8 2
    if w == 0x03 then fail "Bad frequency"
                 else return $ [44100,48000,32000] !! fi w

-- | Currently we only support Stereo and Mono
getMode :: MaybeT BitGet MP3Mode
getMode = do
    w <- lift $ getAsWord8 2
    return $ [Stereo,JointStereo,DualChannel,Mono] !! fi w

getModeExt :: MaybeT BitGet (Bool, Bool)
getModeExt = do
    w <- lift $ getAsWord8 2
    return $ table !! fi w
  where table = [(False, False), (False, True), (True, False), (True, True)]

-- 
-- Side info reading
-- 
-- "Almost" exactly follows the ISO standard
-- | readSideInfo simply parse the sideinformation of an mp3 file
-- the sideInformation is located after the header, and contains
-- values that will be used when doing the decoding. Stereo mode
-- contains four granules, two per channel while the Mono mode contains
-- two granules for the single channel.
unpackSideInfo :: MP3Mode -> Int -> BitGet (SideInfo Int)
unpackSideInfo mode freq = do
    dataptr  <- getInt 9
    skipPrivate
    scaleFactors <- getScaleFactors
    case mode of
        Mono -> do g0 <- getGranule
                   g1 <- getGranule
                   return $ Single freq (dataptr * 8) scaleFactors (g0, g1)
        _    -> do g0 <- getGranule
                   g1 <- getGranule
                   g2 <- getGranule
                   g3 <- getGranule
                   return $ Dual freq (dataptr * 8) scaleFactors (g0, g2) (g1, g3)
  where
    skipPrivate = case mode of
        Mono -> skip 5
        _    -> skip 3
    getScaleFactors = case mode of
        Mono -> replicateM 4 getBit
        _    -> replicateM 8 getBit
    getGranule = do
            part23len        <- getInt 12
            bigValues        <- getInt 9
            globalGain       <- getInt 8
            scaleFacCompress <- getInt 4
            w                <- getBit -- windowSwitching

            blockType  <- getInt $ if w then 2 else 0
            mixedBlock <- if w then getBit else return False
            tableSelect0  <- getInt 5
            tableSelect1  <- getInt 5
            tableSelect2  <- if w then return 0 else getInt 5
            subBlockGain0 <- if w then liftM fi (getInt 3) else return 0
            subBlockGain1 <- if w then liftM fi (getInt 3) else return 0
            subBlockGain2 <- if w then liftM fi (getInt 3) else return 0
            region0Count  <- if w then return (reg0 mixedBlock blockType)
                                else getInt 4
            region1Count  <- if w then return 0 else getInt 3
            --from this point we have some similar code to the one bjorn uses
            let r0count = if w then (if blockType == 2 then 8 else 7)
                             else region0Count
                r1count = if w then 20 - r0count
                             else region1Count
                sbTable   = tableScaleBandBoundary freq
                r1bound   = sbTable $ r0count + 1
                r2bound   = sbTable $ r0count + 1 + r1count + 1
                bv2       = bigValues * 2
                reg0len   = if blockType == 2 then min bv2 36
                               else min bv2 r1bound
                reg1len     = if blockType == 2 then min (bv2-reg0len) 540 
                                 else min (bv2-reg0len) (r2bound - reg0len) 
                reg2len     = if blockType == 2 then 0   
                                 else bv2 - (reg0len + reg1len)
            preFlag           <- getBit
            scaleFacScale     <- getBit
            count1TableSelect <- getBit
            return $ Granule bigValues (mp3FloatRep1 globalGain)
                             scaleFacCompress
                             w blockType mixedBlock tableSelect0
                             tableSelect1 tableSelect2
                             (mp3FloatRep2 subBlockGain0)
                             (mp3FloatRep2 subBlockGain1)
                             (mp3FloatRep2 subBlockGain2)
                             preFlag scaleFacScale count1TableSelect
                             reg0len reg1len reg2len part23len
    reg0 False 2 = 8
    reg0 _ _     = 7

--
-- Stuff for reading frame data
--

-- | readFrameData reads the data belonging to the current frame
-- the data is sometimes (most of the time) splitted by a header
-- that is in the middle of the data.
unpackMainData :: MP3Header Int -> BitGet (MP3H BS.BitString Int)
unpackMainData h1@(MP3Header mode _ fsize hsize sin) = do
    skipId3
    lhs <- getBits $ fi $ dataPointer sin
    (s, maybeH2) <- lookAhead $ do
        s <- safeSkip $ fi fsize
        if s == BOF then do
            hh <- runMaybeT unpackHeader
            return (s, hh) else return (s, Nothing)
    case maybeH2 of
        Just h2 ->  do
            let reads = ((fsize - hsize) - (dataPointer $ sideInfo h2))
            skip $ fi hsize
            rhs <- getBits $ fi reads
            return $ Corr (h1{sideInfo = sin' sin lhs rhs}, h2)
        Nothing -> if s == BOF then do
                     (i,h2) <- lookAhead findHeader
                     case h2 of
                         Nothing -> return Error
                         Just h2' -> do skip (fi $ (fi i) - 
                                             (dataPointer $ sideInfo h2'))
                                        unpackMainData h2'
                     else do
                         rhs <- getBits $ fi fsize -- take as much
                         return $ Last h1{sideInfo = sin' sin lhs rhs}
    where
        mode' = if mode == Mono then 17 else 32
        sin' sin lhs rhs = chopGranule sin $ BS.append lhs rhs

-- | This step is a preparation for the later decoding
-- maybe this should be moved to the next stage so that
-- it can benefit from threading while decoding?
mp3FloatRep1 :: Int -> Double
mp3FloatRep1 n = 2.0 ** (0.25 * (fromIntegral n - 210))

-- The same as above goes for this.
mp3FloatRep2 :: Int -> Double
mp3FloatRep2 n = 2.0 ** (0.25 * (fromIntegral (-n * 8)))

-- Get the data for each granule and save it inside of the
-- granule for the decoding phase
chopGranule ::  SideInfo Int -> BS.BitString -> SideInfo BS.BitString
chopGranule side bits = case side of
    (Single sr p s (g1,g2)) -> flip runBitGet bits $ do
                         g0' <- forGranule g1
                         g1' <- forGranule g2
                         return $ Single sr p s (g0', g1')
    (Dual sr p s (g0, g2) (g1, g3)) -> flip runBitGet bits $ do
                         g0' <- forGranule g0
                         g1' <- forGranule g1
                         g2' <- forGranule g2
                         g3' <- forGranule g3
                         return $ Dual sr p s (g0', g2') (g1', g3')
  where forGranule :: Granule Int -> BitGet (Granule BS.BitString)
        forGranule g = do info <- getBits $ fi $ mp3Data g
                          return $ g {mp3Data = info}

-- handy stuff does not belong here but is used everywhere
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- Scale factor stuff, I think...
tableScaleBandBoundLong :: Int -> [Int]
tableScaleBandBoundLong sr = case sr of
    44100 -> [0,4,8,12,16,20,24,30,36,44,52,62,74,90,110,134,
              162, 196, 238, 288, 342, 418, 576] 
    48000 -> [0,4,8,12,16,20,24,30,36,42,50,60,72,88,106,128, 
              156, 190, 230, 276, 330, 384, 576] 
    32000 -> [0,4,8,12,16,20,24,30,36,44,54,66,82,102,126,156,
              194, 240, 296, 364, 448, 550, 576]
    _     -> error "Wrong samplerate for table"

tableScaleBandBoundary :: Int -> Int -> Int
tableScaleBandBoundary sfreq index = tableScaleBandBoundLong sfreq !! index

