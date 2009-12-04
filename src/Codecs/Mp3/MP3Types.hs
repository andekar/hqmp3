module MP3Types where
import BitGet
import qualified Huffman as Huff
import Data.Bits
import Data.Word
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import ID3
import Control.Monad
import qualified BitString as BITS

type MP3Data a    = (MP3Header a, MP3Header a)
data MP3Mode    = Stereo | JointStereo | DualChannel | Mono deriving (Show,Eq)

-- CMP3Header
data MP3Header a
    = MP3Header { mode      :: MP3Mode
                , mext      :: (Bool,Bool) -- used only with jointstereo
                , fsize     :: Int -- frame  size
                , hsize     :: Int -- header size
                , sideInfo  :: SideInfo a
                }
    deriving Show

-- Derived from page 17 in ISO-11172-3
-- The side info is totalling 17 or 32 bits, mono and stereo, respectively
data SideInfo a
    = Single { sampleRate  :: Int
             , dataPointer :: Int -- 9 bits
             , scales      :: [Bool]
             , gran0       :: Granule a
             , gran1       :: Granule a
             }
    | Dual   { sampleRate  :: Int
             , dataPointer :: Int -- 9 bits
             , scales' :: [Bool]
             , gran0'  :: Granule a
             , gran1'  :: Granule a
             , gran2'  :: Granule a
             , gran3'  :: Granule a
             } deriving Show

instance Functor SideInfo where
    fmap f side = case side of
        (Single _ _ _ g0 g1) -> side {gran0 = fmap f g0, gran1 = fmap f g1}
        (Dual _ _ _ g0 g1 g2 g3) -> side { gran0' = fmap f g0
                                       , gran1' = fmap f g1
                                       , gran2' = fmap f g2
                                       , gran3' = fmap f g3}

-- Granule is computed for each specific channel
data Granule a = Granule {
--     scaleBits         :: Int        -- 12 bits
    bigValues         :: Int        -- 9 bits
  , globalGain        :: Int        -- 8 bits
  , scaleFacCompress  :: Int        -- 4 bits scaleLength?
  , windowSwitching   :: Bool       -- 1 bit
    -- windowzzz
  , blockType       :: Int      -- 2 bits
  , mixedBlock      :: Bool     -- 1 bit
  , tableSelect_1   :: Int      -- 5 bits
  , tableSelect_2   :: Int      -- 5 bits

    -- window == 0
  , tableSelect_3   :: Int

    -- window == 1
  , subBlockGain1   :: Double   -- 3 bits
  , subBlockGain2   :: Double   -- 3 bits
  , subBlockGain3   :: Double   -- 3 bits

  , preFlag           :: Bool       -- 1 bit
  , scaleFacScale     :: Bool       -- 1 bit
  , count1TableSelect :: Bool       -- 1 bit

  -- calculated from previous values
  , region0Start     :: Int
  , region1Start     :: Int
  , region2Start     :: Int

  , mp3Data          :: a
} deriving Show

instance Functor Granule where
    fmap f gran = gran {mp3Data = f (mp3Data gran)}

-- handy stuff does not belong here but is used everywhere
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
