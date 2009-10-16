module BitGet where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as S
import BitUtil
import Data.Word
import Data.Bits
import Debug.Trace

data S = S   S.ByteString -- The first strict part of the bytestring
             L.ByteString -- The rest of the byteString
             Int          -- The bit we're standing at

newtype BitGet a = BitGet { unGet :: S -> (Either String a,S) }

-- Runner!
runBitGet :: BitGet a -> L.ByteString -> Either String a
runBitGet g bs = case unGet g (S sb lb 0) of
    (a,_) -> a
  where
    (sb,lb) = case bs of
        L.Empty           -> (S.empty, L.empty)
        (L.Chunk sb' lb') -> (sb',lb')

-- TODO make the type of BitGet an either, and implement fail
instance Monad BitGet where
    return a = BitGet (\s -> (Right a,s))
    bg >>= f = BitGet $ \s -> case unGet bg s of
        (Right a,s')  -> unGet (f a) s'
        (Left str,s') -> (Left str,s')
    fail str = BitGet (\s -> (Left str,s))

instance Functor BitGet where
    f `fmap` bg = BitGet $ \s -> case unGet bg s of
        (Right a,s')  -> (Right (f a), s')
        (Left err,s') -> (Left err, s')

-- Get the state
get :: BitGet S
get = BitGet $ \s -> (Right s,s)

-- Toss the old state
put :: S -> BitGet ()
put s = BitGet $ \_ -> (Right (),s)

skip :: Int -> BitGet ()
skip i = readN i (const ())

getBit :: BitGet Bool
getBit = getAsWord8 1 >>= return . flip testBit 0

getAsWord8 :: Int -> BitGet Word8
getAsWord8 = flip readN S.head

getAsWord16 :: Int -> BitGet Word16
getAsWord16 i
    | i <= 8    = getAsWord8 i >>= return . fromIntegral
    | otherwise = readN i f
  where 
    f :: S.ByteString -> Word16
    f bs = ((fromIntegral $ S.head bs) `shiftL` 8) .|. (fromIntegral $ S.index bs 1)

lookAhead :: BitGet a -> BitGet a
lookAhead bg = do
    s <- get 
    a <- bg
    put s
    return a

-- Remember that we have to shift all read bytestrings left
-- like... j bits
readN :: Int -> (S.ByteString -> a) -> BitGet a
readN i f = do
    (S sb lb j) <- get
    let s = S.null sb
    if i < (8-j) && not s then do -- we must check if it is empty!!
        let sb'  = S.take 1 sb
            sb'' = rightTruncateBits i (rightShift (8 - (i + j)) sb')
        put $ S sb lb (i + j)
        return $ f sb''
        else if i == (8 - j) then do
            let (sb', sb'') = S.splitAt 1 sb
            case S.null sb'' of
                False -> put $ S sb'' lb 0
                True  -> case lb of
                    L.Empty             -> put $ S sb''  lb 0
                    (L.Chunk sb''' lb') -> put $ S sb''' lb' 0
            return $ f $ rightTruncateBits i sb'
            else do
                let i'  = i + j
                    j'  = i' `mod` 8
                    t   = if j' == 0 then 0 else 1
                    i'' = i' `div` 8 + t
                case S.length sb > i'' of
                    True -> do
                       let sb'  = S.take i'' sb
                           sb'' = S.drop (i'' - t) sb
                       put $ S sb'' lb j' -- this is quite like magic!!
                       return $ f $ rightTruncateBits i (rightShift (8-j') sb')
                       -- seriously, this looks like crap...what was kolmodin
                       -- thinking? splitAt? S.concat? L.toChunks? swamp...c?
                    False -> case L.splitAt (fromIntegral i'') (sb `join` lb) of
                        (consuming, rest) -> do
                        let now = S.concat . L.toChunks $ consuming
                        put $ mkState (now,rest) j'
                        if (S.length now < i'')
                          then fail "Too few bits remain"
                          else return $ f $ rightTruncateBits i (rightShift (8-j') now)
  where
    mkState :: (S.ByteString, L.ByteString) -> Int -> S
    mkState (_,(L.Chunk sb lb)) 0 = S sb lb 0
    mkState (_,L.Empty) 0         = S S.empty L.Empty 0
    mkState (sb,lb) j             = S sb' lb j
        where sb' = S.drop (S.length sb - 1) sb

join :: S.ByteString -> L.ByteString -> L.ByteString
join sb lb
    | S.null sb = lb
    | otherwise = L.Chunk sb lb
