{-# OPTIONS -w #-}

module Decoder (decodeFrame, DChannel(..), ChannelData(..), Scales(..)) where

import BitGet
import qualified Huffman as Huff
import qualified BitString as BITS
import Data.Bits
import Data.Word
import Data.Maybe
import Data.Int
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import ID3
import Control.Monad
import Unpack
import Mp3Trees

import Data.Array.Unboxed
import MP3Types

decodeFrame :: [MP3Header] -> [DChannel Int]
decodeFrame = map ((\(x,y) -> decodeGranules x y) .
                   (\x -> (sideInfo x, mp3Data x)))

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Bjorn Edstrom
tableScaleLength :: Array Int (Int,Int)
tableScaleLength = listArray (0,15)
        [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
         (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)]

data Scales = Scales [Word8] [[Word8]] deriving Show
data ChannelData a = ChannelData Scales [a] deriving Show
data DChannel a 
    = DMono   (Granule, ChannelData a) 
              (Granule, ChannelData a)
    | DStereo (Granule, ChannelData a)
              (Granule, ChannelData a)
              (Granule, ChannelData a)
              (Granule, ChannelData a)
    deriving Show

-- Does decoding of granules given the main_data() chunk
-- This function does huffman decoding, and extracts the 
-- scale factor stuff as described in p.18 in ISO-11172-3
decodeGranules :: SideInfo -> BITS.BitString -> DChannel Int
decodeGranules sideInfo bs = case sideInfo of
    (Single _ scfsi gran1 gran2) -> 
       let [d1,d2] = map (fi . scaleBits) [gran1,gran2]
           (bs1,bs2) = BITS.splitAt d1 bs
           (g1,scale1@(Scales p _)) = decodeGranule [] scfsi gran1 bs1
           (g2,scale2)              = decodeGranule p  scfsi gran2 bs2
       in DMono (gran1, ChannelData scale1 g1)
                (gran2, ChannelData scale2 g2)

    (Dual _ scfsi gran1 gran2 gran3 gran4) ->
       let [d1,d2,d3,d4] = map (fi . scaleBits) [gran1,gran2,gran3,gran4]
           (bs1,bs')    = BITS.splitAt d1 bs
           (bs2, bs'')  = BITS.splitAt d2 bs'
           (bs3, bs''') = BITS.splitAt d3 bs''
           (bs4, _)     = BITS.splitAt d4 bs'''

           (scfsi0,scfsi1) = splitAt 4 scfsi
           (g1,scale1@(Scales p _))  = decodeGranule [] scfsi0 gran1 bs1
           (g2,scale2@(Scales p' _)) = decodeGranule [] scfsi1 gran2 bs2
           (g3,scale3) = decodeGranule p scfsi0 gran3 bs3
           (g4,scale4) = decodeGranule p' scfsi1 gran4 bs4
       in  DStereo (gran1, ChannelData scale1 g1) 
                   (gran2, ChannelData scale2 g2)
                   (gran3, ChannelData scale3 g3)
                   (gran4, ChannelData scale4 g4)

-- Handy stuff
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

decodeGranule :: [Word8] -> [Bool] -> Granule -> BITS.BitString -> ([Int], Scales)
decodeGranule prev scfsi (Granule scaleBits bigValues globalGain
                          scaleFacCompress windowSwitching blockType
                          mixedBlockFlag tableSelect_1 tableSelect_2
                          tableSelect_3 subBlockGain1 subBlockGain2
                          subBlockGain3 preFlag scaleFacScale count1TableSelect
                          r0 r1 r2) dat
    = flip runBitGet dat $ do scales    <- pScaleFactors prev
                              huffData' <- huffData
                              return (huffData', scales)
    where
      t0 = getTree tableSelect_1
      t1 = getTree tableSelect_2
      t2 = getTree tableSelect_3

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
              return $ Scales scalefacL0 (scaleFacS0 ++ scaleFacS1)
          | blockType == 2 && windowSwitching = do
              -- slen1: 0 to 5
              -- slen2: 6 to 11
              scaleFacS0 <- replicateM 6 $ replicateM 3 $ getAsWord8 $ fi slen1
              scaleFacS1 <- replicateM 6 $ replicateM 3 $ getAsWord8 $ fi slen2
              return $ Scales [] (scaleFacS0 ++ scaleFacS1)
          | otherwise = do
              -- slen1: 0 to 10
              s0 <- if c scfsi0 then return $ take 6 prev
                                else replicateM 6 $ getAsWord8 $ fi slen1
              s1 <- if c scfsi1 then return $ take 5 $ drop 6 prev
                                else replicateM 5 $ getAsWord8 $ fi slen1
              -- slen2: 11 to 20
              s2 <- if c scfsi2 then return $ take 5 $ drop 11 prev
                                else replicateM 5 $ getAsWord8 $ fi slen2
              s3 <- if c scfsi3 then return $ take 5 $ drop 16 prev
                                else replicateM 5 $ getAsWord8 $ fi slen2
                  -- here we might need a padding 0 after s3
                  -- (if we want a list of 22 elements)
              return $ Scales (s0 ++ s1 ++ s2 ++ s3) []
                  where c sc = sc && not (null prev)

huffDecode :: [(Int, HuffTable)] -> Bool -> BitGet [Int]
huffDecode [(r0,t0), (r1,t1), (r2,t2)] count1Table = do
    rem'  <- getLength
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
            rest <- if rem > 0 then do
                huffDecodeVWXY huff
                else return []
            return $ v' : w' : x' : y' : rest
        Nothing -> return []
  where setSign 0 = return 0
        setSign c = liftM (\s -> if s then negate c else c) getBit

requantize :: DChannel Int -> DChannel Double
requantize chan = case chan of
    (DMono g1 g2)         -> DMono   (f g1) (f g2)
    (DStereo g1 g2 g3 g4) -> DStereo (f g1) (f g2) (f g3) (f g4)
  where f = uncurry requantizeGran

requantizeGran :: Granule -> ChannelData Int -> (Granule, ChannelData Double)
requantizeGran gran (ChannelData scales xs) = 
        (gran, ChannelData scales $ map (requantizeValue . fromIntegral) xs)
  where
    requantizeValue :: Floating a => a -> a
    requantizeValue x = 
        let subgain = undefined
            gain = undefined
            y = (signum x) * ((abs x) ** (3/4))
            z = 2.0 ** (1/4 * (gain - 210 - 8 * subgain))
            in y * z

