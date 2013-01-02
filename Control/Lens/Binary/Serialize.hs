{-# Language Rank2Types #-}
module Control.Lens.Binary.Serialize where
-- base
import Control.Applicative
import Data.Bits
import Data.Word
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- lens
import Control.Lens
import Data.Bits.Lens
-- data-default
import Data.Default

-- | A class for serializing and deserializing values by way of lenses.
class Serialize b where
  -- | Apply a lens to an action.
  (+>)  :: b a -> Simple Lens s a -> b s
  -- | Sequence two similarly-typed actions.
  (|+|) :: b t -> b t -> b t
  -- | Apply to some number of bytes.
  bytes :: Int -> b ByteString

infixl 4 |+|

-- | Get the nth byte, counting from the high-end and starting from 0.
byteAt :: (Integral b, Bits b) => Int -> Simple Lens b Word8
byteAt i fn b = backward <$> fn forward where
  offset = bitSize b - (i + 1) * 8
  forward = fromIntegral . (.&.) 255 $ b `shiftR` offset
  backward w8 = b .&. complement (255 `shiftL` offset) 
    .|. (fromIntegral w8 `shiftL` offset)
-- I showed this to edwardk and he stuck it in lens;
-- it's due to be released in lens-3.8.0.

-- | Interact with a single byte.
byte :: Serialize s => s Word8
byte = bytes 1 +> \fn w8 -> B.head <$> fn (B.singleton w8)

-- | Interact with a 'Word16' encoded little-endian.
word16le :: Serialize s => s Word16
word16le = byte +> byteAt 0 |+| byte +> byteAt 1

-- | Interact with a 'Word16' encoded big-endian.
word16be :: Serialize s => s Word16
word16be = byte +> byteAt 1 |+| byte +> byteAt 0

-- | Interact with a 'Word32' encoded little-endian.
word32le :: Serialize s => s Word32
word32le = byte +> byteAt 0 |+| byte +> byteAt 1
  |+| byte +> byteAt 2 |+| byte +> byteAt 3

-- | Interact with a 'Word32' encoded big-endian.
word32be :: Serialize s => s Word32
word32be = byte +> byteAt 3 |+| byte +> byteAt 2
  |+| byte +> byteAt 1 |+| byte +> byteAt 0

-- | Interact with a 'Word64' encoded little-endian.
word64le :: Serialize s => s Word64
word64le = byte +> byteAt 0 |+| byte +> byteAt 1
  |+| byte +> byteAt 2 |+| byte +> byteAt 3
  |+| byte +> byteAt 4 |+| byte +> byteAt 5
  |+| byte +> byteAt 6 |+| byte +> byteAt 7

-- | Interact with a 'Word64' encoded big-endian.
word64be :: Serialize s => s Word64
word64be = byte +> byteAt 7 |+| byte +> byteAt 6
  |+| byte +> byteAt 5 |+| byte +> byteAt 4
  |+| byte +> byteAt 3 |+| byte +> byteAt 2
  |+| byte +> byteAt 1 |+| byte +> byteAt 0
