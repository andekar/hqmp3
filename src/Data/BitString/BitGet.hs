module BitGet where

import BitString as BS
import Control.Monad
import Data.Int
import Data.Word
import Prelude hiding (drop, head, splitAt)

newtype BitGet a = BitGet { unGet :: BitString -> (a,BitString) }

instance Monad BitGet where
    return a = BitGet $ \bs -> (a,bs)
    bg >>= f = BitGet $ \bs -> case unGet bg bs of
        ~(a,bs') -> unGet (f a) bs'

runBitGet :: BitGet a -> BitString -> a
runBitGet g bs = case unGet g bs of
    (a, _) -> a

get :: BitGet BitString
get = BitGet $ \bs -> (bs,bs)

put :: BitString -> BitGet ()
put bs = BitGet $ const ((),bs)

getBit :: BitGet Bool
getBit = do
    (bit,bits) <- liftM (splitAt 1) get
    put bits
    return $ head bit

getBits :: Int64 -> BitGet BitString
getBits i = do
    (bits,rest) <- liftM (splitAt i) get
    put rest
    return bits

-- YEAHH HASKELL!!
skip :: Int64 -> BitGet ()
skip i = liftM (drop i) get >>= put

getWord8 :: BitGet Word8
getWord8 = do
    word <- liftM takeWord8 get
    skip 8
    return word

getWord16 :: BitGet Word16
getWord16 = do
    word <- liftM takeWord16 get
    skip 16
    return word

getAsWord8 :: Int64 -> BitGet Word8
getAsWord8 i = do
    r <- liftM (BS.takeAsWord8 (fromIntegral i)) get
    skip i
    return r

getAsWord16 :: Int64 -> BitGet Word16
getAsWord16 i = do
    r <- liftM (BS.takeAsWord16 (fromIntegral i)) get
    skip i
    return r

atLeast :: Int64 -> BitGet Bool
atLeast i = liftM (flip BS.atLeast i) get

lookAhead :: BitGet a -> BitGet a
lookAhead bg = do
    s <- get
    res <- bg
    put s
    return res
 
getRemaining :: BitGet BitString
getRemaining = get

getInt :: Int64 -> BitGet Int
getInt i = do
    s <- liftM (BS.getInt i) get
    skip i
    return s