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
newtype Get r = Get (ByteString -> r -> Maybe (ByteString, r))

instance Serializes Get where
  bytes i = Get $ \bs _ -> if B.length bs < i then Nothing else
    Just (B.drop i bs, B.take i bs)
  Get b %% l = Get $ \bs r -> b bs (view l r) >>=
    \(bs, new) -> return (bs, set l new r)
  Get a %> Get b = Get $ \bs r -> a bs r >>= uncurry b

-- | Deserialize a value, given the value to start with.
deserialize' :: Get b -> b -> ByteString -> Maybe b
deserialize' (Get a) x bs = snd <$> a bs x

-- | Deserialize a value, starting with the default value of its type.
deserialize :: Default b => Get b -> ByteString -> Maybe b
deserialize a bs = deserialize' a def bs
