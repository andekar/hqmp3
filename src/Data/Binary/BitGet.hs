module BitGet where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI
import qualified Data.ByteString as S
import BitUtil
import Data.Word
import Data.Bits
import Debug.Trace
import Control.Arrow
import Control.Monad hiding (join)

data S = S   S.ByteString -- The first strict part of the bytestring
             L.ByteString -- The rest of the byteString
             Int          -- The bit we're standing at

newtype BitGet a = BitGet { unGet :: S -> (a, S) }

-- Runner!
runBitGet :: BitGet a -> L.ByteString -> a
runBitGet = runBitGetAt 0

runBitGetAt :: Int -> BitGet a -> L.ByteString -> a
runBitGetAt pos g bs = case unGet g (S sb lb pos) of
    (a,_) -> a
  where
    ~(sb,lb) = case bs of
        LI.Empty           -> (S.empty, L.empty)
        (LI.Chunk sb' lb') -> (sb',lb')

-- TODO make the type of BitGet an either, and implement fail
instance Monad BitGet where
    return a = BitGet (\s -> (a, s))
    bg >>= f = BitGet $ \s -> case unGet bg s of
        ~(a, s')  -> unGet (f a) s'

instance Functor BitGet where
    f `fmap` bg = BitGet $ \s -> case unGet bg s of
        ~(a, s')  -> (f a, s')

-- TODO
-- instance Applicative BitGet where
--     pure = ap
--     v <*> v' = 

-- Get the state
get :: BitGet S
get = BitGet $ \s -> (s, s)

-- Toss the old state
put :: S -> BitGet ()
put s = BitGet $ const ((), s)

skip :: Int -> BitGet ()
skip = flip unsafeReadN (const ())

safeSkip :: Int -> BitGet Int
safeSkip = liftM snd . flip safeReadN id

atLeast :: Int -> BitGet Bool
atLeast i = do
    (S sb lb j) <- get
    let fs = (8-j) + 8 * S.length sb
    if fs >= i
        then return True
        else return $ checkSize (i - fs) lb
  where
    checkSize :: Int -> L.ByteString -> Bool
    checkSize 0 _       = True
    checkSize _ LI.Empty = False
    checkSize i (LI.Chunk s l)
        | S.length s >= i = True
        | otherwise       = checkSize (i - S.length s) l

getBit :: BitGet Bool
getBit = liftM (flip testBit 0) $ getAsWord8 1

getAsWord8 :: Int -> BitGet Word8
getAsWord8 = flip unsafeReadN S.head

getSafeAsWord8 :: Int -> BitGet (Word8, Int)
getSafeAsWord8 = flip safeReadN S.head

getAsWord16 :: Int -> BitGet Word16
getAsWord16 i
    | i <= 8    = liftM fromIntegral $ getAsWord8 i
    | otherwise = unsafeReadN i f
  where 
    f :: S.ByteString -> Word16
    f bs = (fromIntegral (S.head bs) `shiftL` 8) .|. fromIntegral (S.index bs 1)

getSafeAsWord16 :: Int -> BitGet (Word16, Int)
getSafeAsWord16 i
    | i <= 8    = liftM (first fromIntegral) $ getSafeAsWord8 i
    | otherwise = safeReadN i f
  where 
    f :: S.ByteString -> Word16
    f bs = (fromIntegral (S.head bs) `shiftL` 8)
       .|. fromIntegral (S.index bs 1)

-- not yet safe!!
getLazyByteString :: Int -> BitGet L.ByteString
getLazyByteString i = do (r,j) <-safeReadN i id
                         return $ LI.Chunk r LI.Empty

-- returns the remaining bytestring
-- it also includes which bit we are currently on
getRemainingLazy :: BitGet (L.ByteString, Int)
getRemainingLazy = do (S sb lb i) <- get
                      return (LI.Chunk sb lb, i)

lookAhead :: BitGet a -> BitGet a
lookAhead bg = do
    s <- get 
    a <- bg
    put s
    return a

-- Remember that we have to shift all read bytestrings left
-- like... j bits
safeReadN :: Int -> (S.ByteString -> a) -> BitGet (a, Int)
safeReadN i f = do
    (S sb lb j) <- get
    let s = S.null sb
    if i < (8-j) && not s then do -- we must check if it is empty!!
        let sb'  = S.take 1 sb
            sb'' = rightTruncateBits i (rightShift (8 - (i + j)) sb')
        put $ S sb lb (i + j)
        return (f sb'', i)
        else if i == (8 - j) then do
            let (sb', sb'') = S.splitAt 1 sb
            case S.null sb'' of
                False -> put $ S sb'' lb 0
                True  -> case lb of
                    LI.Empty             -> put $ S sb''  lb 0
                    (LI.Chunk sb''' lb') -> put $ S sb''' lb' 0
            return (f $ rightTruncateBits i sb', i)
            else do
                let i'  = i + j
                    j'  = i' `mod` 8
                    t   = if j' == 0 then 0 else 1
                    i'' = i' `div` 8 + t
                case S.length sb > i'' of
                    True -> do
                       let sb'  = S.take i'' sb
                           sb'' = S.drop (i'' - t) sb
                           ll   = rightTruncateBits i (rightShift (8-j') sb')
                       put $ S sb'' lb j' -- this is quite like magic!!
                       return (f ll, i)
                       -- seriously, this looks like crap...what was kolmodin
                       -- thinking? splitAt? S.concat? L.toChunks? swamp...c?
                    False -> case L.splitAt (fromIntegral i'') (sb `join` lb) of
                        (consuming, rest) -> do
                        let now = S.concat . L.toChunks $ consuming
                        put $ mkState (now,rest) j'
                        if S.length now < i''
                          then let ll = (S.length now * 8) - j
                                   now' = rightTruncateBits j now
                               in return (f now', ll)
                          else let ll = rightTruncateBits i (rightShift (8-j')
                                                             now)
                               in return (f ll, i)

unsafeReadN :: Int -> (S.ByteString -> a) -> BitGet a
unsafeReadN i f = do
    (S sb lb j) <- get
    let s = S.null sb
        g | i < (8-j) && not s = do
            let sb'  = S.take 1 sb
                sb'' = rightTruncateBits i (rightShift (8 - (i + j)) sb')
            put $ S sb lb (i + j)
            return $ f sb''
        
          | i == (8 - j) = do
            let (sb', sb'') = S.splitAt 1 sb
            case S.null sb'' of
                False -> put $ S sb'' lb 0
                True  -> case lb of
                    LI.Empty             -> put $ S sb''  lb 0
                    (LI.Chunk sb''' lb') -> put $ S sb''' lb' 0
            return $ f (rightTruncateBits i sb')
          | otherwise = do
            let i'  = i + j
                j'  = i' `mod` 8
                t   = if j' == 0 then 0 else 1
                i'' = i' `div` 8 + t
            case S.length sb > i'' of
                True -> do
                   let sb'  = S.take i'' sb
                       sb'' = S.drop (i'' - t) sb
                       ll   = rightTruncateBits i (rightShift (8-j') sb')
                   put $ S sb'' lb j' -- this is quite like magic!!
                   return $ f ll
                   -- seriously, this looks like crap...what was kolmodin
                   -- thinking? splitAt? S.concat? L.toChunks? swamp...c?
                False -> case L.splitAt (fromIntegral i'') (sb `join` lb) of
                    (consuming, rest) -> do
                    let now = S.concat . L.toChunks $ consuming
                    put $ mkState (now,rest) j'
                    if S.length now < i''
                      then error "You should know better, use the safeReadN instead"
                      else let ll = rightTruncateBits i (rightShift (8-j')
                                                         now)
                           in return $ f ll
    g

mkState :: (S.ByteString, L.ByteString) -> Int -> S
mkState (_,(LI.Chunk sb lb)) 0 = S sb lb 0
mkState (_,LI.Empty) 0         = S S.empty LI.Empty 0
mkState (sb,lb) j             = S sb' lb j
    where sb' = S.drop (S.length sb - 1) sb

join :: S.ByteString -> L.ByteString -> L.ByteString
join sb lb
    | S.null sb = lb
    | otherwise = LI.Chunk sb lb

getInt :: Int -> BitGet Int
getInt i
    | i <= 8    = liftM fromIntegral $ getAsWord8 i
    | i <= 16   = liftM fromIntegral $ getAsWord16 i
--    | i <= 32   = getAsWord32 i >>= return . fromIntegral
    | otherwise = error "64 bits does not fit into an Int"