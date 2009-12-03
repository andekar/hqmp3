{-# OPTIONS -w #-}

module Decoder (decodeFrames, ChannelData(..), Scales(..)) where

import BitGet
import qualified Huffman as Huff
import qualified BitString as BS
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Int
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import ID3
import Control.Monad
import Control.Arrow
import Unpack
import Mp3Trees
import Debug.Trace

import Data.Array.Unboxed
import MP3Types

-- Scales [long blocks] [[windows for short blocks]]
data Scales        = Scales { long :: [Word8] 
                            , short :: [[Word8]]
                            }
    deriving Show
data ChannelData a = ChannelData { scale :: Scales
                                 , chanData :: a}
    deriving Show

instance Functor ChannelData where
    f `fmap` ChannelData sc a = ChannelData sc (f a)

type DChannel a = SideInfo (ChannelData a)
decodeFrames :: [MP3Header BS.BitString] -> [DChannel [Double]]
decodeFrames hs = reordered -- synthehised
  where
    -- steps in decoding, done in this order
    unpacked    = map (decodeGranules . sideInfo) hs
    requantized = zipWith requantize freqs unpacked
    reordered   = map mp3Reorder requantized
--    unaliased   = map reduceAliases reordered
--    synthehised = map (synthMore . synthIMCDT) unaliased
    -- variables needed above
    freqs  = map (sampleRate . sideInfo) hs

-- Okee
mp3Reorder :: DChannel [Double] -> DChannel [Double]
mp3Reorder sInfo = case sInfo of
    (Single sr a b g0 g1) -> Single sr a b (reorder sr g0) (reorder sr g1)
    (Dual sr a b g0 g1 g2 g3) -> Dual sr a b  (reorder sr g0) (reorder sr g1)
                                             (reorder sr g2) (reorder sr g3)
  where reorder sr g
             | blockType g == 2 && windowSwitching g
             = g {mp3Data = ChannelData  (scale $ mp3Data g) $
                  take 46 (chanData $ mp3Data g) ++
                  drop 36 (freq' sr (chanData $ mp3Data g))}
             | blockType g == 2 = g {mp3Data = 
                                     ChannelData (scale $ mp3Data g)
                                     (freq' sr (chanData $ mp3Data g))}
             | otherwise = g

        -- We want the output list to be as long as the input list to 
        -- correctly handle IS Stereo decoding, but the unsafe reorderList 
        -- requires the input to be as long as the index list.
        freq' sr ds  = take (length ds) $ 
                       reorderList (tableReorder sr) (padWith 576 0.0 ds)

-- 'reorderList' takes a list of indices and a list of elements and
-- reorders the list based on the indices. Example:
-- reorderList [1,2,0] [a,b,c] == [b,c,a]
reorderList :: [Int] -> [a] -> [a]
reorderList indices list = map (list !!) indices

reduceAliases :: DChannel [Double] -> DChannel [Double]
reduceAliases = undefined

synthMore :: DChannel [Double] -> DChannel [Double]
synthMore = undefined

synthIMCDT :: DChannel [Double] -> DChannel [Double]
synthIMCDT = undefined

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
       let (g0,scale0) = decodeGranule [] scfsi gran0
           (g1,scale1) = decodeGranule (long scale0)  scfsi gran1
       in single { gran0 = gran0 {mp3Data = (ChannelData scale0 g0)}
                 , gran1 = gran1 {mp3Data = (ChannelData scale1 g1)}}

    dual@(Dual _ _ scfsi gran0 gran1 gran2 gran3) ->
       let (scfsi0,scfsi1) = splitAt 4 scfsi
           (g0,scale0) = decodeGranule [] scfsi0 gran0
           (g1,scale1) = decodeGranule [] scfsi1 gran1
           (g2,scale2) = decodeGranule (long scale1) scfsi0 gran2
           (g3,scale3) = decodeGranule (long scale2) scfsi1 gran3
       in  dual { gran0' = gran0 {mp3Data = (ChannelData scale0 g0)}
                , gran1' = gran1 {mp3Data = (ChannelData scale1 g1)}
                , gran2' = gran2 {mp3Data = (ChannelData scale2 g2)}
                , gran3' = gran3 {mp3Data = (ChannelData scale3 g3)}}

-- Handy stuff
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

decodeGranule :: [Word8] -> [Bool] -> Granule BS.BitString -> ([Int], Scales)
decodeGranule prev scfsi (Granule bigValues globalGain
                          scaleFacCompress windowSwitching blockType
                          mixedBlockFlag tableSelect_0 tableSelect_1
                          tableSelect_2 subBlockGain1 subBlockGain2
                          subBlockGain3 preFlag scaleFacScale count1TableSelect
                          r0 r1 r2 mp3Data)
    = flip runBitGet mp3Data $ do scales    <- pScaleFactors prev
                                  huffData' <- huffData
                                  return (huffData', scales)
    where
      t0 = getTree tableSelect_0
      t1 = getTree tableSelect_1
      t2 = getTree tableSelect_2

      huffData = huffDecode [(r0,t0), (r1, t1), (r2, t2)] count1TableSelect
      (slen1, slen2) = tableScaleLength ! scaleFacCompress
      (scfsi0:scfsi1:scfsi2:scfsi3:[]) = scfsi
      
      -- will return a list of long scalefactors
      -- a list of lists of short scalefactors
      -- an int describing how much we read, this might not be needed
      pScaleFactors :: [Word8] -> BitGet Scales
      pScaleFactors prev
              -- as defined in page 18 of the mp3 iso standard
          | blockType == 2 && mixedBlockFlag && windowSwitching = do
              -- slen1: 0 to 7       (long  window scalefactor band)
              -- slen1: bands 3 to 5 (short window scalefactor band)
              -- slen2: bands 6 to 11
              scalefacL0 <- replicateM 8 $ getAsWord8 $ fi slen1
              scaleFacS0 <- replicateM 3 $ replicateM 3 $ getAsWord8 $ fi slen1
              scaleFacS1 <- replicateM 7 $ replicateM 3 $ getAsWord8 $ fi slen2
              let scaleS = [[0,0,0],[0,0,0],[0,0,0]] ++ scaleFacS0 ++ scaleFacS1
              return $ Scales (padWith 22 0 scalefacL0) 
                              (padWith 22 [0,0,0] scaleS)
          | blockType == 2 && windowSwitching = do
              -- slen1: 0 to 5
              -- slen2: 6 to 11
              scaleFacS0 <- replicateM 6 $ replicateM 3 $ getAsWord8 $ fi slen1
              scaleFacS1 <- replicateM 6 $ replicateM 3 $ getAsWord8 $ fi slen2
              return $ Scales [] (padWith 22 [0,0,0] $ scaleFacS0 ++ scaleFacS1)
          | otherwise = do
              -- slen1: 0 to 10
              s0 <- if recycle scfsi0 then return $ take 6 prev
                                else replicateM 6 $ getAsWord8 $ fi slen1
              s1 <- if recycle scfsi1 then return $ take 5 $ drop 6 prev
                                else replicateM 5 $ getAsWord8 $ fi slen1
              -- slen2: 11 to 20
              s2 <- if recycle scfsi2 then return $ take 5 $ drop 11 prev
                                else replicateM 5 $ getAsWord8 $ fi slen2
              s3 <- if recycle scfsi3 then return $ take 5 $ drop 16 prev
                                else replicateM 5 $ getAsWord8 $ fi slen2
                  -- here we might need a padding 0 after s3
                  -- (if we want a list of 22 elements)
              return $ Scales (s0 ++ s1 ++ s2 ++ s3 ++ [0]) []
                  where recycle sc = sc && not (null prev)

-- swamp...c?
huffDecode :: [(Int, HuffTable)] -> Bool -> BitGet [Int]
huffDecode [(r0,t0), (r1,t1), (r2,t2)] count1Table = do
    r0res <- liftM concat $ replicateM (r0 `div` 2) $ huffDecodeXY t0
    r1res <- liftM concat $ replicateM (r1 `div` 2) $ huffDecodeXY t1
    r2res <- liftM concat $ replicateM (r2 `div` 2) $ huffDecodeXY t2
    quadr <- huffDecodeVWXY (getQuadrTree count1Table)
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
            rest <- if rem > 0 then huffDecodeVWXY huff
                               else return []
            return $ v' : w' : x' : y' : rest
        Nothing -> return []
  where setSign 0 = return 0
        setSign c = liftM (\s -> if s then negate c else c) getBit

-- 
-- Requantization below
--

requantize :: Int -> DChannel [Int] -> DChannel [Double]
requantize freq chan = case chan of
    (Single sr a b g1 g2)    -> Single sr a b (f g1) (f g2)
    (Dual sr a b g1 g2 g3 g4) -> Dual sr a b (f g1) (f g2) (f g3) (f g4)
  where f = requantizeGran freq
        

requantizeGran :: Int -> Granule (ChannelData [Int]) -> (Granule (ChannelData [Double]))
requantizeGran freq gran
    = gran {mp3Data =  ChannelData scales $ requantizeValues xs}
  where
    (ChannelData scales@(Scales longScales shortScales) xs)
              = mp3Data gran
    requantizeValues :: [Int] -> [Double]
    requantizeValues compressed
        | blockflag == 2 && mixedflag = take 36 long ++ drop 36 short
        | blockflag == 2 && winSwitch = short
        | otherwise                   = long
        where 
            blockflag = blockType gran
            mixedflag = mixedBlock gran
            winSwitch = windowSwitching gran
            gain      = fromIntegral $ globalGain gran
            subgain1  = subBlockGain1 gran
            subgain2  = subBlockGain2 gran
            subgain3  = subBlockGain3 gran

            long  = zipWith procLong  compressed longbands
            short = zipWith procShort compressed shortbands

            procLong :: Int -> Int -> Double
            procLong sample sfb = 
                let localgain   = fi $ longScales !! sfb
                    dsample     = fromIntegral sample :: Double
                in gain * localgain * dsample **^ (4/3)

            procShort :: Int -> (Int,Int) -> Double
            procShort sample (sfb, win) =
                let localgain = fi $ (shortScales !! sfb) !! win
                    blockgain = case win of 0 -> subgain1
                                            1 -> subgain2
                                            2 -> subgain3
                    dsample   = fromIntegral sample
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

-- CODE BELOW TAKEN FROM BJORN

tableScaleBandBoundLong :: Int -> [Int]
tableScaleBandBoundLong 44100 = [ 0,   4,   8,  12,  16,  20,  24,  30,
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



-- Tables for reordering below

tableReorder :: Int -> [Int]
tableReorder 44100 = 
    [ 0,   1,   2,   3,  12,  13,   4,   5,   6,   7,  16,  17,   8,   9,  10,
    11,  20,  21, 14,  15,  24,  25,  26,  27,  18,  19,  28,  29,  30,  31,
    22,  23,  32,  33,  34,  35, 36,  37,  38,  39,  48,  49,  40,  41,  42,
    43,  54,  55,  44,  45,  46,  47,  60,  61, 50,  51,  52,  53,  66,  67,
    56,  57,  58,  59,  74,  75,  62,  63,  64,  65,  82,  83, 68,  69,  70,
    71,  72,  73,  76,  77,  78,  79,  80,  81,  84,  85,  86,  87,  88,  89,
    90,  91,  92,  93,  94,  95, 100, 101, 102, 103, 104, 105, 110, 111, 112,
    113, 114, 115, 96,  97,  98,  99, 120, 121, 106, 107, 108, 109, 132, 133,
    116, 117, 118, 119, 144, 145, 122, 123, 124, 125, 126, 127, 134, 135, 136,
    137, 138, 139, 146, 147, 148, 149, 150, 151, 128, 129, 130, 131, 156, 157,
    140, 141, 142, 143, 170, 171, 152, 153, 154, 155, 184, 185, 158, 159, 160,
    161, 162, 163, 172, 173, 174, 175, 176, 177, 186, 187, 188, 189, 190, 191,
    164, 165, 166, 167, 168, 169, 178, 179, 180, 181, 182, 183, 192, 193, 194,
    195, 196, 197, 198, 199, 200, 201, 202, 203, 216, 217, 218, 219, 220, 221,
    234, 235, 236, 237, 238, 239, 204, 205, 206, 207, 208, 209, 222, 223, 224,
    225, 226, 227, 240, 241, 242, 243, 244, 245, 210, 211, 212, 213, 214, 215,
    228, 229, 230, 231, 232, 233, 246, 247, 248, 249, 250, 251, 252, 253, 254,
    255, 256, 257, 274, 275, 276, 277, 278, 279, 296, 297, 298, 299, 300, 301,
    258, 259, 260, 261, 262, 263, 280, 281, 282, 283, 284, 285, 302, 303, 304,
    305, 306, 307, 264, 265, 266, 267, 268, 269, 286, 287, 288, 289, 290, 291,
    308, 309, 310, 311, 312, 313, 270, 271, 272, 273, 318, 319, 292, 293, 294,
    295, 348, 349, 314, 315, 316, 317, 378, 379, 320, 321, 322, 323, 324, 325,
    350, 351, 352, 353, 354, 355, 380, 381, 382, 383, 384, 385, 326, 327, 328,
    329, 330, 331, 356, 357, 358, 359, 360, 361, 386, 387, 388, 389, 390, 391,
    332, 333, 334, 335, 336, 337, 362, 363, 364, 365, 366, 367, 392, 393, 394,
    395, 396, 397, 338, 339, 340, 341, 342, 343, 368, 369, 370, 371, 372, 373,
    398, 399, 400, 401, 402, 403, 344, 345, 346, 347, 408, 409, 374, 375, 376,
    377, 464, 465, 404, 405, 406, 407, 520, 521, 410, 411, 412, 413, 414, 415,
    466, 467, 468, 469, 470, 471, 522, 523, 524, 525, 526, 527, 416, 417, 418,
    419, 420, 421, 472, 473, 474, 475, 476, 477, 528, 529, 530, 531, 532, 533,
    422, 423, 424, 425, 426, 427, 478, 479, 480, 481, 482, 483, 534, 535, 536,
    537, 538, 539, 428, 429, 430, 431, 432, 433, 484, 485, 486, 487, 488, 489,
    540, 541, 542, 543, 544, 545, 434, 435, 436, 437, 438, 439, 490, 491, 492,
    493, 494, 495, 546, 547, 548, 549, 550, 551, 440, 441, 442, 443, 444, 445,
    496, 497, 498, 499, 500, 501, 552, 553, 554, 555, 556, 557, 446, 447, 448,
    449, 450, 451, 502, 503, 504, 505, 506, 507, 558, 559, 560, 561, 562, 563,
    452, 453, 454, 455, 456, 457, 508, 509, 510, 511, 512, 513, 564, 565, 566,
    567, 568, 569, 458, 459, 460, 461, 462, 463, 514, 515, 516, 517, 518, 519,
    570, 571, 572, 573, 574, 575] 

tableReorder 48000 = 
  [ 0,   1,   2,   3,  12,  13,   4,   5,   6,   7,  16,  17,   8,   9,  10,
    11,  20,  21, 14,  15,  24,  25,  26,  27,  18,  19,  28,  29,  30,  31,
    22,  23,  32,  33,  34,  35, 36,  37,  38,  39,  48,  49,  40,  41,  42,
    43,  54,  55,  44,  45,  46,  47,  60,  61, 50,  51,  52,  53,  66,  67,
    56,  57,  58,  59,  72,  73,  62,  63,  64,  65,  78,  79, 68,  69,  70,
    71,  84,  85,  74,  75,  76,  77,  94,  95,  80,  81,  82,  83, 104, 105,
    86,  87,  88,  89,  90,  91,  96,  97,  98,  99, 100, 101, 106, 107, 108,
    109, 110, 111, 92,  93, 114, 115, 116, 117, 102, 103, 126, 127, 128, 129,
    112, 113, 138, 139, 140, 141, 118, 119, 120, 121, 122, 123, 130, 131, 132,
    133, 134, 135, 142, 143, 144, 145, 146, 147, 124, 125, 150, 151, 152, 153,
    136, 137, 164, 165, 166, 167, 148, 149, 178, 179, 180, 181, 154, 155, 156,
    157, 158, 159, 168, 169, 170, 171, 172, 173, 182, 183, 184, 185, 186, 187,
    160, 161, 162, 163, 192, 193, 174, 175, 176, 177, 208, 209, 188, 189, 190,
    191, 224, 225, 194, 195, 196, 197, 198, 199, 210, 211, 212, 213, 214, 215,
    226, 227, 228, 229, 230, 231, 200, 201, 202, 203, 204, 205, 216, 217, 218,
    219, 220, 221, 232, 233, 234, 235, 236, 237, 206, 207, 240, 241, 242, 243,
    222, 223, 260, 261, 262, 263, 238, 239, 280, 281, 282, 283, 244, 245, 246,
    247, 248, 249, 264, 265, 266, 267, 268, 269, 284, 285, 286, 287, 288, 289,
    250, 251, 252, 253, 254, 255, 270, 271, 272, 273, 274, 275, 290, 291, 292,
    293, 294, 295, 256, 257, 258, 259, 300, 301, 276, 277, 278, 279, 326, 327,
    296, 297, 298, 299, 352, 353, 302, 303, 304, 305, 306, 307, 328, 329, 330,
    331, 332, 333, 354, 355, 356, 357, 358, 359, 308, 309, 310, 311, 312, 313,
    334, 335, 336, 337, 338, 339, 360, 361, 362, 363, 364, 365, 314, 315, 316,
    317, 318, 319, 340, 341, 342, 343, 344, 345, 366, 367, 368, 369, 370, 371,
    320, 321, 322, 323, 324, 325, 346, 347, 348, 349, 350, 351, 372, 373, 374,
    375, 376, 377, 378, 379, 380, 381, 382, 383, 444, 445, 446, 447, 448, 449,
    510, 511, 512, 513, 514, 515, 384, 385, 386, 387, 388, 389, 450, 451, 452,
    453, 454, 455, 516, 517, 518, 519, 520, 521, 390, 391, 392, 393, 394, 395,
    456, 457, 458, 459, 460, 461, 522, 523, 524, 525, 526, 527, 396, 397, 398,
    399, 400, 401, 462, 463, 464, 465, 466, 467, 528, 529, 530, 531, 532, 533,
    402, 403, 404, 405, 406, 407, 468, 469, 470, 471, 472, 473, 534, 535, 536,
    537, 538, 539, 408, 409, 410, 411, 412, 413, 474, 475, 476, 477, 478, 479,
    540, 541, 542, 543, 544, 545, 414, 415, 416, 417, 418, 419, 480, 481, 482,
    483, 484, 485, 546, 547, 548, 549, 550, 551, 420, 421, 422, 423, 424, 425,
    486, 487, 488, 489, 490, 491, 552, 553, 554, 555, 556, 557, 426, 427, 428,
    429, 430, 431, 492, 493, 494, 495, 496, 497, 558, 559, 560, 561, 562, 563,
    432, 433, 434, 435, 436, 437, 498, 499, 500, 501, 502, 503, 564, 565, 566,
    567, 568, 569, 438, 439, 440, 441, 442, 443, 504, 505, 506, 507, 508, 509,
    570, 571, 572, 573, 574, 575]

tableReorder 32000 = 
    [ 0,   1,   2,   3,  12,  13,   4,   5,   6,   7,  16,  17,   8,   9,  10,
    11,  20,  21, 14,  15,  24,  25,  26,  27,  18,  19,  28,  29,  30,  31,
    22,  23,  32,  33,  34,  35, 36,  37,  38,  39,  48,  49,  40,  41,  42,
    43,  54,  55,  44,  45,  46,  47,  60,  61, 50,  51,  52,  53,  66,  67,
    56,  57,  58,  59,  74,  75,  62,  63,  64,  65,  82,  83, 68,  69,  70,
    71,  72,  73,  76,  77,  78,  79,  80,  81,  84,  85,  86,  87,  88,  89,
    90,  91,  92,  93,  94,  95, 102, 103, 104, 105, 106, 107, 114, 115, 116,
    117, 118, 119, 96,  97,  98,  99, 100, 101, 108, 109, 110, 111, 112, 113,
    120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 142, 143, 144,
    145, 146, 147, 158, 159, 160, 161, 162, 163, 132, 133, 134, 135, 136, 137,
    148, 149, 150, 151, 152, 153, 164, 165, 166, 167, 168, 169, 138, 139, 140,
    141, 174, 175, 154, 155, 156, 157, 194, 195, 170, 171, 172, 173, 214, 215,
    176, 177, 178, 179, 180, 181, 196, 197, 198, 199, 200, 201, 216, 217, 218,
    219, 220, 221, 182, 183, 184, 185, 186, 187, 202, 203, 204, 205, 206, 207,
    222, 223, 224, 225, 226, 227, 188, 189, 190, 191, 192, 193, 208, 209, 210,
    211, 212, 213, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
    260, 261, 262, 263, 264, 265, 286, 287, 288, 289, 290, 291, 240, 241, 242,
    243, 244, 245, 266, 267, 268, 269, 270, 271, 292, 293, 294, 295, 296, 297,
    246, 247, 248, 249, 250, 251, 272, 273, 274, 275, 276, 277, 298, 299, 300,
    301, 302, 303, 252, 253, 254, 255, 256, 257, 278, 279, 280, 281, 282, 283,
    304, 305, 306, 307, 308, 309, 258, 259, 312, 313, 314, 315, 284, 285, 346,
    347, 348, 349, 310, 311, 380, 381, 382, 383, 316, 317, 318, 319, 320, 321,
    350, 351, 352, 353, 354, 355, 384, 385, 386, 387, 388, 389, 322, 323, 324,
    325, 326, 327, 356, 357, 358, 359, 360, 361, 390, 391, 392, 393, 394, 395,
    328, 329, 330, 331, 332, 333, 362, 363, 364, 365, 366, 367, 396, 397, 398,
    399, 400, 401, 334, 335, 336, 337, 338, 339, 368, 369, 370, 371, 372, 373,
    402, 403, 404, 405, 406, 407, 340, 341, 342, 343, 344, 345, 374, 375, 376,
    377, 378, 379, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419,
    456, 457, 458, 459, 460, 461, 498, 499, 500, 501, 502, 503, 420, 421, 422,
    423, 424, 425, 462, 463, 464, 465, 466, 467, 504, 505, 506, 507, 508, 509,
    426, 427, 428, 429, 430, 431, 468, 469, 470, 471, 472, 473, 510, 511, 512,
    513, 514, 515, 432, 433, 434, 435, 436, 437, 474, 475, 476, 477, 478, 479,
    516, 517, 518, 519, 520, 521, 438, 439, 440, 441, 442, 443, 480, 481, 482,
    483, 484, 485, 522, 523, 524, 525, 526, 527, 444, 445, 446, 447, 448, 449,
    486, 487, 488, 489, 490, 491, 528, 529, 530, 531, 532, 533, 450, 451, 452,
    453, 454, 455, 492, 493, 494, 495, 496, 497, 534, 535, 536, 537, 538, 539,
    540, 541, 542, 543, 544, 545, 552, 553, 554, 555, 556, 557, 564, 565, 566,
    567, 568, 569, 546, 547, 548, 549, 550, 551, 558, 559, 560, 561, 562, 563,
    570, 571, 572, 573, 574, 575]


padWith :: Int -> a -> [a] -> [a]
padWith n pad xs = xs ++ replicate (n - length xs) pad
