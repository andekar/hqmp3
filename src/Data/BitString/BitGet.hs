module BitGet where

import BitString as BS
import Control.Monad
import Data.Int
import Data.Word
import Prelude hiding (drop, head, splitAt)
import Control.Monad.Trans
import Control.Monad.Identity
import Control.Applicative

newtype BitGetT m a = BitGetT { unGet :: BitString -> m (a, BitString) }

instance (Monad m) => Functor (BitGetT m) where
    fmap f m = BitGetT $ \s -> do 
        ~(x,s') <- unGet m s
        return (f x, s')

instance MonadTrans BitGetT where
    lift m = BitGetT $ \s -> do
        a <- m
        return (a,s)

instance (Monad m) => Monad (BitGetT m) where
    return a = BitGetT $ \bs -> return (a,bs)
    bg >>= f = BitGetT $ \bs -> do
        ~(a,bs') <- unGet bg bs
        unGet (f a) bs'

instance (Monad m) => Applicative (BitGetT m) where
    pure a = return a
    (<*>) = ap

type BitGet a = BitGetT Identity a

runBitGet :: BitGet a -> BitString -> a
runBitGet g bs = case runIdentity (unGet g bs) of
    (a, _) -> a

get :: (Monad m) => BitGetT m BitString
get = BitGetT $ \bs -> return (bs,bs)

put :: (Monad m) => BitString -> BitGetT m ()
put bs = BitGetT $ const $ return ((),bs)

getBit :: (Monad m) => BitGetT m Bool
getBit = do
    (bit,bits) <- liftM (splitAt 1) get
    put bits
    return $ head bit

getBits :: (Monad m) => Int64 -> BitGetT m BitString
getBits i = do
    (bits,rest) <- liftM (splitAt i) get
    put rest
    return bits

-- YEAHH HASKELL!!
skip :: (Monad m) => Int64 -> BitGetT m ()
skip i = (liftM (drop i) get) >>= put

getWord8 :: (Monad m) => BitGetT m Word8
getWord8 = do
    word <- liftM takeWord8 get
    skip 8
    return word

getWord16 :: (Monad m) => BitGetT m Word16
getWord16 = do
    word <- liftM takeWord16 get
    skip 16
    return word

getAsWord8 :: (Monad m) => Int64 -> BitGetT m Word8
getAsWord8 i = do
    r <- liftM (BS.takeAsWord8 (fromIntegral i)) get
    skip i
    return r

getAsWord16 :: (Monad m) => Int64 -> BitGetT m Word16
getAsWord16 i = do
    r <- liftM (BS.takeAsWord16 (fromIntegral i)) get
    skip i
    return r

lookAhead :: (Monad m) => BitGetT m a -> BitGetT m a
lookAhead bg = do
    s <- get
    res <- bg
    put s
    return res
 
getRemaining :: (Monad m) => BitGetT m BitString
getRemaining = get

getInt :: (Monad m) => Int64 -> BitGetT m Int
getInt i = do
    s <- liftM (BS.getInt i) get
    skip i
    return s

getLength :: (Monad m) => BitGetT m Int64
getLength = liftM BS.length get

getAtLeast :: (Monad m) => Int64 -> BitGetT m Bool
getAtLeast i = liftM (flip BS.atLeast i) get