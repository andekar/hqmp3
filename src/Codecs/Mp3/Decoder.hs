{-# OPTIONS -w #-}

module Codecs.Mp3.Decoder (decodeFrames
                          , DChannel
                          , ChannelData(..)
                          , Scales(..)) where

import Data.Binary.BitString.BitGet
import qualified Codec.Compression.Huffman.Huffman as Huff
import qualified Data.Binary.BitString.BitString as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Codecs.Mp3.ID3
import Control.Monad
import Control.Arrow
import Codecs.Mp3.Unpack
import Codecs.Mp3.Mp3Trees
import qualified Control.Monad.State.Lazy as LS
import Data.Array.Unboxed
import Codecs.Mp3.MP3Types
import Codecs.Mp3.HybridFilterBank
import Codecs.Mp3.Tables
import Codecs.Mp3.Types

data Scales
    = Scales { long :: [Double]
             , short :: [[Double]] }
    deriving Show

data ChannelData a
    = ChannelData { scale :: Scales
                  , chanData :: a }
    deriving Show

instance Functor ChannelData where
    f `fmap` ChannelData sc a = ChannelData sc (f a)

type DChannel a = SideInfo (ChannelData a)

decodeFrames :: [MP3Header BS.BitString] -> [([Double],[Double])]
decodeFrames hs = output
  where
    -- steps in decoding, done in this order
    unpacked    = map (decodeGranules . sideInfo) hs
    requantized = zipWith requantize freqs unpacked
    reordered   = map mp3Reorder requantized

    output =  LS.evalState (decodeRest reordered) emptyMP3DecodeState
    -- variables needed
    freqs  = map (sampleRate . sideInfo) hs

-- Does the "step1" as in bjorns decoder :(
-- Note that we skip stereoIS and StereoMS
decodeRest :: [DChannel [Double]]
        -> LS.State MP3DecodeState [([Double],[Double])]
decodeRest []  = return []
decodeRest (chan:xs) = do
  prevState <- LS.get
  case chan of
    (Single _ _ _ g0 g1) -> do 
        let (state0, output0) = step1 g0 (decodeState0 prevState)
            (state1, output1) = step1 g1 state0
            state = MP3DecodeState state1 state1
        LS.put $ MP3DecodeState state1 state1
        rest <- decodeRest xs
        return ((output0 ++ output1, output0 ++ output1):rest)
    (Dual _ _ _ g0 g1 g2 g3) -> do
        let (state0, output0) = step1 g0 (decodeState0 prevState)
            (state1, output1) = step1 g1 (decodeState1 prevState)
            (state2, output2) = step1 g2 state0
            (state3, output3) = step1 g3 state1
            state =  MP3DecodeState state2 state3
        LS.put $ MP3DecodeState state2 state3
        rest <- decodeRest xs
        return ((output0 ++ output2, output1 ++ output3) : rest )
  where
    step1 gran state =
        let bf  = toBlockflag (mixedBlock gran) (blockType gran)
            bt  = blockType gran
            inp = chanData $ mp3Data gran
        in (mp3HybridFilterBank bf bt state inp)

-- Sadly this is needed because of Bjorns structure
toBlockflag mixedflag blocktype
        | mixedflag == True = MixedBlocks
        | blocktype == 2 = ShortBlocks
        | otherwise = LongBlocks

data MP3DecodeState = MP3DecodeState {
    decodeState0 :: MP3HybridState,
    decodeState1 :: MP3HybridState
} deriving Show

emptyMP3DecodeState :: MP3DecodeState
emptyMP3DecodeState = MP3DecodeState emptyMP3HybridState emptyMP3HybridState

-- Okee
mp3Reorder :: DChannel [Double] -> DChannel [Double]
mp3Reorder sInfo = case sInfo of
    (Single sr a b g0 g1)     -> Single sr a b (reorder sr g0) (reorder sr g1)
    (Dual sr a b g0 g1 g2 g3) -> Dual sr a b (reorder sr g0) (reorder sr g1)
                                            (reorder sr g2) (reorder sr g3)
  where reorder sr g
             | mixedBlock g
             = g {mp3Data = ChannelData  (scData g) $
                  take 46 (chData g) ++
                  drop 36 (freq' sr $ chData g)}
             | blockType g == 2 = g {mp3Data = 
                                     ChannelData (scData g)
                                     (freq' sr $ chData g)} -- short blocks
             | otherwise = g -- long blocks
        f = chanData . mp3Data
        -- We want the output list to be as long as the input list to 
        -- correctly handle IS Stereo decoding, but the unsafe reorderList 
        -- requires the input to be as long as the index list.
        freq' sr ds  = take (length ds) $ 
                       reorderList (tableReorder sr) (padWith 576 0.0 ds)
        chData = chanData . mp3Data
        scData = scale . mp3Data

-- 'reorderList' takes a list of indices and a list of elements and
-- reorders the list based on the indices. Example:
-- reorderList [1,2,0] [a,b,c] == [b,c,a]
reorderList :: [Int] -> [a] -> [a]
reorderList indices list = map (list !!) indices

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Bjorn Edstrom
tableScaleLength :: Array Int (Int,Int)
tableScaleLength = listArray (0,15)
        [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
         (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)]


-- Does decoding of granules given the main_data() chunk
-- This function does huffman decoding, and extracts the 
-- scale factor stuff as described in p.18 in ISO-11172-3
decodeGranules :: SideInfo BS.BitString -> DChannel [Int]
decodeGranules sideInfo = case sideInfo of
    single@(Single _ _ scfsi gran0 gran1) -> 
       let (g0,scale0@(l,s)) = decodeGranule [] scfsi gran0
           (g1,scale1) = decodeGranule l scfsi gran1
       in  single { gran0 = setChannel scale0 g0 gran0
                  , gran1 = setChannel scale1 g1 gran1}

    dual@(Dual _ _ scfsi gran0 gran1 gran2 gran3) ->
       let (scfsi0,scfsi1) = splitAt 4 scfsi
           (g0,scale0@(l0,_)) = decodeGranule [] scfsi0 gran0
           (g1,scale1@(l1,_)) = decodeGranule [] scfsi1 gran1
           (g2,scale2@(l2,_)) = decodeGranule l0 scfsi0 gran2
           (g3,scale3@(l3,_)) = decodeGranule l1 scfsi1 gran3
       in  dual { gran0' = setChannel scale0 g0 gran0
                , gran1' = setChannel scale1 g1 gran1
                , gran2' = setChannel scale2 g2 gran2
                , gran3' = setChannel scale3 g3 gran3}

  where setChannel s g gr = gr {mp3Data = (ChannelData (scales s g gr) g)}
        scales s g gr = unpack s (cpress gr) (sscale gr)
        cpress = preFlag
        sscale = scaleFacScale
        unpack = mp3UnpackScaleFactors

decodeGranule :: [Int] -> [Bool] -> Granule BS.BitString
                  -> ([Int], ([Int],[[Int]]))
decodeGranule prev scfsi (Granule _ _
                          scaleFacCompress window blockType blockFlag ts_0 ts_1
                          ts_2 _ _ _ preFlag scaleFacScale count1TableSelect
                          r0 r1 r2 mp3Data)
    = flip runBitGet mp3Data $ do (l,s)   <- pScaleFactors prev
                                  huffData' <- huffData
                                  return (huffData', (l,s))
    where
      t0 = getTree ts_0 -- tableSelect
      t1 = getTree ts_1
      t2 = getTree ts_2

      huffData = huffDecode [(r0,t0), (r1, t1), (r2, t2)] count1TableSelect
      (slen1, slen2) = tableScaleLength ! scaleFacCompress
      (scfsi0:scfsi1:scfsi2:scfsi3:[]) = scfsi
      
      -- will return a list of long scalefactors
      -- a list of lists of short scalefactors
      -- an int describing how much we read, this might not be needed
      pScaleFactors :: [Int] -> BitGet ([Int],[[Int]])
      pScaleFactors prev
              -- as defined in page 18 of the mp3 iso standard
          | blockType == 2 && blockFlag && window = do
              -- slen1: 0 to 7       (long  window scalefactor band)
              -- slen1: bands 3 to 5 (short window scalefactor band)
              -- slen2: bands 6 to 11
              scalefacL0 <- replicateM 8 $ getAsWord8 $ fi slen1
              scaleFacS0 <- replicateM 3 $ replicateM 3 $ getAsWord8 $ fi slen1
              scaleFacS1 <- replicateM 7 $ replicateM 3 $ getAsWord8 $ fi slen2
              let scaleS= [[0,0,0],[0,0,0],[0,0,0]] ++ scaleFacS0 ++ scaleFacS1
              return ((map fi $  padWith 22 0 scalefacL0),
                      map (map fi) (padWith 22 [0,0,0] scaleS))
          | blockType == 2 && window = do
              -- slen1: 0 to 5
              -- slen2: 6 to 11
              scaleFacS0 <- replicateM 6 $ replicateM 3 $ getAsWord8 $ fi slen1
              scaleFacS1 <- replicateM 6 $ replicateM 3 $ getAsWord8 $ fi slen2
              return$ ([], map (map fi)
                       (padWith 22 [0,0,0] $ scaleFacS0 ++ scaleFacS1))
          | otherwise = do
              -- slen1: 0 to 10
              s0 <- if recycle scfsi0 then return $ take 6 prev
                       else replicateM 6 $ liftM fi $ getAsWord8 $ fi slen1
              s1 <- if recycle scfsi1 then return $ take 5 $ drop 6 prev
                       else replicateM 5 $ liftM fi $ getAsWord8 $ fi slen1
              -- slen2: 11 to 20
              s2 <- if recycle scfsi2 then return $ take 5 $ drop 11 prev
                       else replicateM 5 $ liftM fi $ getAsWord8 $ fi slen2
              s3 <- if recycle scfsi3 then return $ take 5 $ drop 16 prev
                       else replicateM 5 $ liftM fi $ getAsWord8 $ fi slen2
                  -- here we might need a padding 0 after s3
                  -- (if we want a list of 22 elements)
              return ((s0 ++ s1 ++ s2 ++ s3 ++ [0]), [[]])
                  where recycle sc = sc && not (null prev)

-- Above parses the scale factors to bits. This functions takes the bits
-- and converts them to doubles according to the correct floating point
-- representation. (See discussion at mp3FloatRep1 above).
--
-- Preflag is simply a predefined table that may be set to give the
-- higher scale factors a larger range than 4 bits.
mp3UnpackScaleFactors :: ([Int], [[Int]]) -> Bool -> Bool -> Scales
mp3UnpackScaleFactors (large, small) preflag scalefacbit =
    let large'  = if not preflag  then large
                          else zipWith (+) large tablePretab
        large'' = map floatFunc large'
        small'  = map (map floatFunc) small
    in Scales large'' small'
    where
        floatFunc = mp3FloatRep3 (fromEnum scalefacbit)

-- Two different floating point representations can be used for the scale
-- factors. (See discussion at mp3FloatRep1).
mp3FloatRep3 :: Int -> Int -> Double
mp3FloatRep3 0 n = 2.0 ** ((-0.5) * (fi n))
mp3FloatRep3 1 n = 2.0 ** ((-1)   * (fi n))

huffDecode :: [(Int, HuffTable)] -> Bool -> BitGet [Int]
huffDecode [(r0,t0), (r1,t1), (r2,t2)] count1Table = do
    r0res <- liftM concat $ replicateM (r0 `div` 2) $ huffDecodeXY t0
    r1res <- liftM concat $ replicateM (r1 `div` 2) $ huffDecodeXY t1
    r2res <- liftM concat $ replicateM (r2 `div` 2) $ huffDecodeXY t2
    quadr <- huffDecodeVWXY (getQuadrTree (not count1Table)) -- why do we need to negate this value?!?!
    return $ r0res ++ r1res ++ r2res ++ quadr

huffDecodeXY :: HuffTable -> BitGet [Int]
huffDecodeXY (huff, linbits) = do
    Just (x,y) <- Huff.decode huff return 0
    x' <- linsign x linbits
    y' <- linsign y linbits
    return $ x' : [y']
  where linsign :: Int -> Int -> BitGet Int
        linsign c l
            | c == 15 && l > 0 = do
                res  <- liftM (+15) $ getInt (fi l)
                liftM (\s -> if s then negate res else res) getBit
            | c > 0 = liftM (\s -> if s then negate c else c) getBit
            | otherwise = return c

huffDecodeVWXY :: Huff.HuffTree (Int,Int,Int,Int) -> BitGet [Int]
huffDecodeVWXY huff = do
    ret <- Huff.decode huff return 0
    case ret of
        (Just (v,w,x,y)) -> do
            v' <- setSign v
            w' <- setSign w
            x' <- setSign x
            y' <- setSign y
            rem <- getLength
            rest <- if rem > 0 then huffDecodeVWXY huff else return []
            return $ v' : w' : x' : y' : rest
        Nothing -> return []
  where setSign 0 = return 0
        setSign c = liftM (\s -> if s then negate c else c) getBit

-- 
-- Requantization below
--

-- seems to be working!
requantize :: Int ->  DChannel [Int] -> DChannel [Double]
requantize _ chan = case chan of
    (Single sr a b g1 g2)    -> Single sr a b (f sr g1) (f sr g2)
    (Dual sr a b g1 g2 g3 g4) -> 
               Dual sr a b (f sr g1) (f sr g2) (f sr g3) (f sr g4)
  where f sr = requantizeGran sr

requantizeGran :: Int -> Granule (ChannelData [Int]) ->
                  (Granule (ChannelData [Double]))
requantizeGran freq gran
    = gran {mp3Data =  ChannelData scales $ requantizeValues xs}
  where
    (ChannelData scales@(Scales longScales shortScales) xs)
              = mp3Data gran
    requantizeValues :: [Int] -> [Double]
    requantizeValues compressed
        | mixedflag = take 36 long ++ drop 36 short
        | blockflag == 2 = short
        | otherwise = long
        where
            blockflag = blockType gran
            mixedflag = mixedBlock gran
            winSwitch = windowSwitching gran
            gain      = globalGain gran
            subgain1  = subBlockGain1 gran
            subgain2  = subBlockGain2 gran
            subgain3  = subBlockGain3 gran

            long  = zipWith procLong  compressed longbands
            short = zipWith procShort compressed shortbands

            procLong :: Int -> Int -> Double
            procLong sample sfb = 
                let localgain   = longScales !! sfb
                    dsample     = fi sample :: Double
                in gain * localgain * dsample **^ (4/3)

            procShort :: Int -> (Int,Int) -> Double
            procShort sample (sfb, win) =
                let localgain = (shortScales !! sfb) !! win
                    blockgain = case win of 0 -> subgain1
                                            1 -> subgain2
                                            2 -> subgain3
                    dsample   = fi sample
                in gain * localgain * blockgain * dsample **^ (4/3)
                                    
            -- Frequency index (0-575) to scale factor band index (0-21).
            longbands = tableScaleBandIndexLong freq
            -- Frequency index to scale factor band index and window index (0-2).
            shortbands = tableScaleBandIndexShort freq

-- b **^ e == sign(b) * abs(b)**e
(**^) :: (Floating a, Ord a) => a -> a -> a
b **^ e = let sign = if b < 0 then -1 else 1
              b'   = abs b
          in sign * b' ** e
infixr 8 **^

-- Code taken from bjorn...
-- TODO: These will have to be altered to deal with arrays instead...
--       One might want to alter the arrays, in which case we want to have
--           mutable arrays in the ST monad instead

tableScaleBandIndexLong :: Int -> [Int]
tableScaleBandIndexLong = indexify . consecutiveDiff . tableScaleBandBoundLong
  where indexify xs = concat (zipWith replicate xs [0..])

tableScaleBandIndexShort :: Int -> [(Int, Int)]
tableScaleBandIndexShort = indexifyWindows . 
                           consecutiveDiff . 
                           tableScaleBandBoundShort
  where
    indexifyWindows = concat . zipWith addFirst [0..] . map buildTriple
    addFirst n = map (\x -> (n, x))
    buildTriple n = replicate n 0 ++ replicate n 1 ++ replicate n 2

consecutiveDiff :: Num a => [a] -> [a]
consecutiveDiff xs = zipWith (-) (tail xs) xs

padWith :: Int -> a -> [a] -> [a]
padWith n pad xs = xs ++ replicate (n - length xs) pad

-- padWith :: UArray a a -> 
