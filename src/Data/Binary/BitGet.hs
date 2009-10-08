module BitGet where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import Data.Word

data S = S   S.ByteString -- The first lazy part of the bytestring
             L.ByteString -- The rest of the byteString
             Int        -- The bit we're standing at

newtype BitGet a = BitGet { unGet :: S -> (a,S) }

-- Runner!
runBitGet :: BitGet a -> S -> a
runBitGet g s = case unGet g s of
    (a,_) -> a

instance Monad BitGet where
    return a = BitGet (\s -> (a,s))
    bg >>= f = BitGet $ \s -> case unGet bg s of
        (a,s') -> unGet (f a) s'

instance Functor BitGet where
    f `fmap` bg = BitGet $ \s -> case unGet bg s of
        (a,s') -> (f a, s')

-- Get the state
get :: BitGet S
get = BitGet $ \s -> (s,s)

-- Toss the old state
put :: S -> BitGet ()
put s = BitGet $ \_ -> ((),s)

skip :: Int -> BitGet ()
skip i = readN i (const ())

getAsWord8 :: Int -> BitGet Word8
getAsWord8 i = readN i L.head

getAsWord16 :: Int -> BitGet Word16
getAsWord16 i = undefined

lookAhead :: BitGet a -> BitGet a
lookAhead bg = do
    s <- get 
    a <- bg
    put s
    return a

-- Read N bits into a bytestring, and convert to some type
readN :: Int -> (L.ByteString -> a) -> BitGet a
readN i f = do
    (S sb lb j) <- get
    case i < (S.length sb * 8 - (8-j)) of
        True | i < (8-j)  -> undefined
             | i == (8-j) -> undefined
             | otherwise  -> undefined
        False -> undefined 

-- Join two bytestrings
join :: S.ByteString -> L.ByteString -> L.ByteString
join sb lb = undefined
    
