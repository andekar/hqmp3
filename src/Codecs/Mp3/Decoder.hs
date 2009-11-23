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

data Scales = Scales [Word8] [[Word8]] deriving Show
data ChannelData = ChannelData Scales [(Int, Int)] deriving Show
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
           (bs1,bs2) = BITS.splitAt d1 bs
--            bs2 = tNdrop d2 d1 bs
           (g1,scale1@(Scales p _),_)   = decodeGranule [] scfsi gran1 bs1
           (g2,scale2,_)                = decodeGranule p  scfsi gran2 bs2
       in DMono (ChannelData scale1 g1) (ChannelData scale2 g2)

    (Dual _ scfsi gran1 gran2 gran3 gran4) ->
       let [d1,d2,d3,d4] = map (fi . scaleBits) [gran1,gran2,gran3,gran4]
           (bs1,bs')    = BITS.splitAt d1 bs
           (bs2, bs'')  = BITS.splitAt d2 bs'
           (bs3, bs''') = BITS.splitAt d3 bs''
           (bs4, _)     = BITS.splitAt d4 bs'''

           br = (BITS.length bs1 +
                 BITS.length bs2 +
                 BITS.length bs3 +
                 BITS.length bs4)
                 >= (d1 + d2 + d3 + d4)

           (scfsi0,scfsi1) = splitAt 4 scfsi
           (g1,scale1@(Scales p _),s)   = decodeGranule [] scfsi0 gran1 bs1
           (g2,scale2@(Scales p' _),s') = decodeGranule [] scfsi1 gran2 bs2
           (g3,scale3,ss)   = decodeGranule p scfsi0 gran3 bs3
           (g4,scale4,ss')  = decodeGranule p' scfsi1 gran4 bs4
       in  DStereo (ChannelData scale1 g1) (ChannelData scale2 g2)
                   (ChannelData scale3 g3) (ChannelData scale4 g4)
  where
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
      t0 = getTree tableSelect_1
      t1 = getTree tableSelect_2
      t2 = getTree tableSelect_3

      -- TODO, count1: how big is it?
      huffData = huffDecode [(r0,t0), (r1, t1), (r2, t2)] (count1TableSelect, 0)
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

huffDecode :: [(Int, HuffTable)] -> (Bool, Int) -> BitGet [(Int, Int)]
huffDecode [(r0,t0), (r1,t1), (r2,t2)] (count1Table,count1) = do
    rem'  <- getLength
    r0res <- replicateM (r0 `div` 2) $ huffDecodeXY t0
    r1res <- replicateM (r1 `div` 2) $ huffDecodeXY t1
    r2res <- replicateM (r2 `div` 2) $ huffDecodeXY t2
    rem   <- getLength
    quadr <- huffDecodeVWXY (getQuadrTree count1Table) -- return () --if rem > 0 then huffDecodeVWXY (getQuadrTree count1Table)
--                         else return []
    return $ rem `seq` r0res ++ r1res ++ r2res

huffDecodeXY :: HuffTable -> BitGet (Int,Int)
huffDecodeXY (huff, linbits) = do
    Just (x,y) <- Huff.decode huff return 0
    x' <- linsign x linbits
    y' <- linsign y linbits
    return (x', y')
  where linsign :: Int -> Int -> BitGet Int
        linsign c l
            | c == 15 && l > 0 = do
                res  <- liftM (+15) $ getInt (fi l)
                liftM (\s -> if s then negate res else res) getBit
            | c > 0 = liftM (\s -> if s then negate c else c) getBit
            | otherwise = return c

huffDecodeVWXY :: Huff.HuffTree (Int,Int,Int,Int) -> BitGet [(Int,Int,Int,Int)]
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
            return ((v',w',x',y'):rest)
        Nothing -> return []
  where setSign 0 = return 0
        setSign c = liftM (\s -> if s then negate c else c) getBit
