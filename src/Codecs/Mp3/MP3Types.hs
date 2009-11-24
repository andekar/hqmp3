module MP3Types where
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
import qualified BitString as BITS

type MP3Data = (MP3Header, EMP3Header)
type MP3Header = CMP3Header BITS.BitString
type EMP3Header = CMP3Header ()
data MP3Mode = Stereo | JointStereo | DualChannel | Mono deriving (Show,Eq)
data CMP3Header a
    = MP3Header { bitRate   :: Int
                , frequency :: Int
                , padding   :: Bool
                , mode      :: MP3Mode
                , mext      :: (Bool,Bool) -- used only with jointstereo
                , fsize     :: Int -- frame  size
                , hsize     :: Int -- header size
                , sideInfo  :: SideInfo
                , mp3Data   :: a -- BITS.BitString
} deriving Show

-- Derived from page 17 in ISO-11172-3
-- The side info is totalling 17 or 32 bits, mono and stereo, respectively
data SideInfo
    = Single { dataPointer :: Int -- 9 bits
             , scales  :: [Bool]
             , gran1   :: Granule
             , gran2   :: Granule }
    | Dual   { dataPointer :: Int -- 9 bits
             , scales' :: [Bool]
             , gran1'  :: Granule
             , gran2'  :: Granule
             , gran3'  :: Granule
             , gran4'  :: Granule } deriving Show

-- Granule is computed for each specific channel
data Granule = Granule {
    scaleBits         :: Int        -- 12 bits
  , bigValues         :: Int        -- 9 bits
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
  , subBlockGain1   :: Int      -- 3 bits
  , subBlockGain2   :: Int      -- 3 bits
  , subBlockGain3   :: Int      -- 3 bits

  , preFlag           :: Bool       -- 1 bit
  , scaleFacScale     :: Bool       -- 1 bit
  , count1TableSelect :: Bool       -- 1 bit

  -- calculated from precious values
  , region0Start     :: Int
  , region1Start     :: Int
  , region2Start     :: Int
} deriving Show

