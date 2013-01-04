module Control.Lens.Binary.Size where
-- lens
import Control.Lens
-- binary-lens
import Control.Lens.Binary.Serialize

-- | An instance of 'Serializes' for finding the expected length
-- of some @Serializes s => s a@.
newtype Size n = Size
  { sizeOf :: Integer }
  deriving (Eq, Show)

instance Serializes Size where
  bytes = Size . fromIntegral
  Size a %> Size b = Size $ a + b
  Size a %% _ = Size a
