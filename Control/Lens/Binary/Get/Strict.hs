module Control.Lens.Binary.Get.Strict where
-- base
import Control.Applicative
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- lens
import Control.Lens
-- data-default
import Data.Default
-- binary-lens
import Control.Lens.Binary.Serialize

-- | Parse a bytestring into some kind of 'Serialize'.
newtype Get r = Get (r -> ByteString -> Maybe (r, ByteString))

instance Serializes Get where
  bytes i = Get $ \_ bs -> if B.length bs < i then Nothing else
    Just (B.take i bs, B.drop i bs)
  Get b %% l = Get $ \r bs -> b (view l r) bs >>=
    \(new, bs) -> return (set l new r, bs)
  Get a %> Get b = Get $ \r bs -> a r bs >>= uncurry b

-- | Deserialize a value, given the value to start with.
deserialize' :: Get b -> b -> ByteString -> Maybe b
deserialize' (Get a) x bs = fst <$> a x bs

-- | Deserialize a value, starting with the default value of its type.
deserialize :: Default b => Get b -> ByteString -> Maybe b
deserialize a bs = deserialize' a def bs
