{-# OPTIONS -w #-}

module Decoder (decodeFrame) where

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
import Debug.Trace
import Control.Monad
import Unpack
import Mp3Trees

import Data.Array.Unboxed
import MP3Types

decodeFrame :: [MP3Header] -> [DChannel]
decodeFrame = map ((\(x,y) -> decodeGranules x y) .
                   (\x -> (sideInfo x, mp3Data x)))

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Bjorn Edstrom
tableScaleLength :: Array Int (Int,Int)
tableScaleLength = listArray (0,15)
        [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
         (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)]

data Scales = Scales [Word8] [[Word8]] Int
    deriving Show
data ChannelData = ChannelData Scales [(Int, Int)]
    deriving Show
data DChannel
    = DMono   ChannelData ChannelData 
    | DStereo ChannelData ChannelData ChannelData ChannelData
    deriving Show

-- Does decoding of granules given the main_data() chunk
-- This function does huffman decoding, and extracts the 
-- scale factor stuff as described in p.18 in ISO-11172-3
decodeGranules :: SideInfo -> BITS.BitString -> DChannel
decodeGranules sideInfo bs = case sideInfo of
    (Single _ scfsi gran1 gran2) -> 
       let [d1,d2] = map (fi . scaleBits) [gran1,gran2]
           bs1 = take' d1 bs
           bs2 = tNdrop d2 d1 bs
           (g1,scale1@(Scales p _ _),_) = decodeGranule [] scfsi gran1 bs1
           (g2,scale2,_)                = decodeGranule p  scfsi gran2 bs2
       in DMono (ChannelData scale1 g1) (ChannelData scale2 g2)

    (Dual _ scfsi gran1 gran2 gran3 gran4) ->
       let [d1,d2,d3,d4] = map (fi . scaleBits) [gran1,gran2,gran3,gran4]
           bs1 = take' d1 bs
           bs2 = trace ("\nbs1 " ++ show d1) tNdrop d2 d1 bs
           bs3 = trace ("\nbs2 " ++ show d2) tNdrop d3 (d1+d2) bs
           bs4 = trace ("\nbs3 " ++ show d3) tNdrop d4 (d1+d2+d3) bs

           (scfsi0,scfsi1) = trace ("\nbs4 " ++ show d4)splitAt 4 scfsi
           (g1,scale1@(Scales p _ _),_)  = decodeGranule [] scfsi0 gran1 bs1
           (g2,scale2@(Scales p' _ _),_) = decodeGranule [] scfsi1 gran2 bs2
           (g3,scale3,_)  = decodeGranule p scfsi0 gran3 bs3
           (g4,scale4,_)  = decodeGranule p' scfsi1 gran4 bs4
       in DStereo (ChannelData scale1 g1) (ChannelData scale2 g2)
                  (ChannelData scale3 g3) (ChannelData scale4 g4)
  where
      take' = BITS.take
      tNdrop :: Int64 -> Int64 -> BITS.BitString -> BITS.BitString
      tNdrop s t bs = BITS.take s $ BITS.drop t bs

-- Handy stuff
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

decodeGranule :: [Word8] -> [Bool] -> Granule -> BITS.BitString
                  -> ([(Int,Int)], Scales, BITS.BitString)
decodeGranule prev scfsi (Granule scaleBits bigValues globalGain
                          scaleFacCompress windowSwitching blockType
                          mixedBlockFlag tableSelect_1 tableSelect_2
                          tableSelect_3 subBlockGain1 subBlockGain2
                          subBlockGain3 preFlag scaleFacScale count1TableSelect
                          r0 r1 r2) dat
    = flip runBitGet dat $ do scales     <- pScaleFactors prev
                              huffData'  <- huffData
                              rest       <- getRemaining
                              return (huffData', scales, rest)
    where
          t0 = trace ("\nTable0: " ++ show tableSelect_1) getTree tableSelect_1
          t1 = trace ("\nTable1: " ++ show tableSelect_2) getTree tableSelect_2
          t2 = trace ("\nTable2: " ++ show tableSelect_3) getTree tableSelect_3

        -- TODO, count1: how big is it?
          huffData = huffDecode [(r0,t0), (r1, t1), (r2, t2)]
                    (count1TableSelect, 0)
          (slen1, slen2) = tableScaleLength ! scaleFacCompress
          (scfsi0:scfsi1:scfsi2:scfsi3:[]) = scfsi
          -- will return a list of long scalefactors
          -- a list of lists of short scalefactors
          -- an int describing how much we read, this might not be needed
          pScaleFactors :: [Word8] -> BitGet Scales
          pScaleFactors  prev
                  -- as defined in page 18 of the mp3 iso standard
              | blockType == 2 && mixedBlockFlag = do
                  -- slen1: 0 to 7 (long window scalefactor band)
                  scalefacL0 <- replicateM 8 $ getAsWord8 (fromIntegral slen1)
                  -- slen1: bands 3 to 5 (short window scalefactor band)
                  scaleFacS0 <- replicateM 3 $ replicateM 3 $ 
                                               getAsWord8 (fromIntegral slen1)
                  -- slen2: bands 6 to 11
                  scaleFacS1 <- replicateM 6 $ replicateM 3 $ 
                                               getAsWord8 (fromIntegral slen2)
                  let length = 17 * slen1 + 18 * slen2
                  -- here we must insert 3 lists of all zeroes since the
                  -- slen1 starts at band 3
                      sr = replicate 3 $ replicate 3 0
                  return $ trace ("\n#####scales: " ++ show length ++ "\n" )$ Scales scalefacL0 (sr ++ scaleFacS0 ++ scaleFacS1) length
              | blockType == 2 = do
                  -- slen1: 0 to 5
                  scaleFacS0 <- replicateM 6 $ replicateM 3 $ 
                                               getAsWord8 (fromIntegral slen1)
                  -- slen2: 6 to 11
                  scaleFacS1 <- replicateM 6 $ replicateM 3 $ 
                                               getAsWord8 (fromIntegral slen2)
                  let length = 18 * slen1 + 18 * slen2
                  return $ trace ("\n#####scales: " ++ show length ++ "\n" )$ Scales [] (scaleFacS0 ++ scaleFacS1) length
              | otherwise = do
                  -- slen1: 0 to 10
                  s0 <- if c scfsi0 then
                            replicateM 6 $ getAsWord8 (fromIntegral slen1)
                            else return $ take 6 prev
                  s1 <- if c scfsi1 then
                            replicateM 5 $ getAsWord8 $ fromIntegral slen1
                            else return $ take 5 $ drop 6 prev
                  -- slen2: 11 to 20
                  s2 <- if c scfsi2 then
                            replicateM 5 $ getAsWord8 $ fromIntegral slen2
                            else return $ take 6 $ drop 11 prev
                  s3 <- if c scfsi3 then
                            replicateM 5 $ getAsWord8 $ fromIntegral slen2
                            else return $ take 6 $ drop 16 prev
                  let length = 6 * (if c scfsi0 then slen1 else 0) +
                               5 * (if c scfsi1 then slen1 else 0) +
                               5 * (if c scfsi2 then slen1 else 0) +
                               5 * (if c scfsi3 then slen1 else 0)
                      -- here we might need a paddig 0 after s3
                      -- (if we want a list of 22 elements)
                  return $ trace ("\n#####scales: " ++ show length ++ "\n" )$Scales (s0 ++ s1 ++ s2 ++ s3) [] length
                      where c sc = not sc && not (null prev)

type HuffTree = (Huff.HuffTree (Int, Int), Int)

huffDecode :: [(Int, HuffTree)] -> (Bool, Int) -> BitGet [(Int, Int)]
huffDecode [(r0,t0), (r1,t1), (r2,t2)] (count1Table,count1) = do
    r0res <- trace ("\nr0len " ++ show r0 ++
                    "\nr1len " ++ show r1 ++
                    "\nr2len " ++ show r2) replicateM (r0 `div` 2) $ huffDecodeXY t0
    rem <- getLength
    r1res <- trace ("weee")$ replicateM (r1 `div` 2) $ huffDecodeXY t1
    r2res <- replicateM (r2 `div` 2) $ huffDecodeXY t2
--     quadr <- replicateM (count1 `div` 4) $ huffDecodeVWXY (getQuadrTree count1Table)
    return (  r1res) --  ++ r2res

huffDecodeXY :: HuffTree -> BitGet (Int,Int)
huffDecodeXY (huff, linbits) = do
    Just (x,y) <- Huff.decode huff return 0
    x' <- linsign x linbits
    y' <- linsign y linbits
    return (x', y')
  where linsign :: Int -> Int -> BitGet Int
        linsign c l 
            | abs c == 15 && l > 0 = do
                res <- getInt (fromIntegral l) >>= \r -> return (r+15)
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
