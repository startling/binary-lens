{-# Language Rank2Types #-}
module Control.Lens.Binary.Serialize where
-- base
import Control.Applicative
import Data.Word
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- lens
import Control.Lens
-- data-default
import Data.Default
-- QuickCheck
import Test.QuickCheck

-- | A class for serializing and deserializing values by way of lenses.
class Serialize b where
  -- | Apply a lens to an action.
  (+>)  :: b a -> Simple Lens s a -> b s
  -- | Sequence two similarly-typed actions.
  (|+|) :: b t -> b t -> b t
  -- | Apply to some number of bytes.
  bytes :: Int -> b ByteString

infixl 4 |+|

-- | Interact with a single byte.
byte :: Serialize s => s Word8
byte = bytes 1 +> \fn w8 -> B.head <$> fn (B.singleton w8)
