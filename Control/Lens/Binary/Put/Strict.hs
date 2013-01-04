module Control.Lens.Binary.Put.Strict where
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- lens
import Control.Lens
-- binary-lens
import Control.Lens.Binary.Serialize

-- | Build a bytestring from some 'Serialize'.
newtype Put r = Put (r -> ByteString)

instance Serializes Put where
  bytes i = Put $ B.take i
  Put b %% l= Put $ b . view l
  Put a %> Put b = Put $ \bs -> a bs `B.append` b bs

-- | Serialize some value, given a Put action.
serialize :: Put a -> a -> ByteString
serialize (Put a) = a
