{-# OPTIONS -w #-}

module Main (decodeFrame) where

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
import Unpack

import Data.Array.Unboxed
import MP3Types

decodeFrame = undefined

-- Array to find out how much data we're supposed to read from
-- scalefac_l. Idea stolen from Bjorn Edstrom
tableScaleLength :: Array Int (Int,Int)
tableScaleLength = listArray (0,15)
        [(0,0), (0,1), (0,2), (0,3), (3,0), (1,1), (1,2), (1,3),
         (2,1), (2,2), (2,3), (3,1) ,(3,2), (3,3), (4,2), (4,3)]

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
                              huffData'  <- huffData
                              (rest,_)   <- getRemainingLazy
                              return (huffData', wedontcare, rest)
    where
          huffData = huffDecode (region0start, region1start, region2start)
                                undefined undefined
-- huffDecode :: (Int, Int, Int) -> (HuffTree, HuffTree, HuffTree)
--               -> Int -> BitGet [(Int, Int)]
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