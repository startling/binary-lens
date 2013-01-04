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
class Serializes b where
  -- | Apply a lens to an action.
  (%%)  :: b a -> Simple Lens s a -> b s
  -- | Sequence two similarly-typed actions.
  (%>) :: b t -> b t -> b t
  -- | Apply to some number of bytes.
  bytes :: Int -> b ByteString

infixl 4 %>

-- | A class for things that have one canonical representation using 'Serialize'.
class Binary b where
  binary :: Serializes s => s b

instance Binary Word8 where
  binary = bytes 1 %% \fn w8 -> B.head <$> fn (B.singleton w8)

-- | A class for things that have two representations using 'Serialize'
-- -- one little-endian and one big-endian.
class Endian b where
  little :: Serializes s => s b
  big    :: Serializes s => s b

instance Endian Word16 where
  little = binary %% byteAt 0 %> binary %% byteAt 1
  big = binary %% byteAt 1 %> binary %% byteAt 0

instance Endian Word32 where
  little = binary %% byteAt 0 %> binary %% byteAt 1
    %> binary %% byteAt 2 %> binary %% byteAt 3
  big = binary %% byteAt 3 %> binary %% byteAt 2
    %> binary %% byteAt 1 %> binary %% byteAt 0

instance Endian Word64 where
  little = binary %% byteAt 0 %> binary %% byteAt 1
    %> binary %% byteAt 2 %> binary %% byteAt 3
    %> binary %% byteAt 4 %> binary %% byteAt 5
    %> binary %% byteAt 6 %> binary %% byteAt 7
  big = binary %% byteAt 7 %> binary %% byteAt 6
    %> binary %% byteAt 5 %> binary %% byteAt 4
    %> binary %% byteAt 3 %> binary %% byteAt 2
    %> binary %% byteAt 1 %> binary %% byteAt 0
