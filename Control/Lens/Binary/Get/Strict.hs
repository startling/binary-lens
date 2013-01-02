module Control.Lens.Binary.Get.Strict where
-- base
import Control.Applicative
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- mtl
import Control.Monad.State
-- lens
import Control.Lens
-- data-default
import Data.Default
-- binary-lens
import Control.Lens.Binary.Serialize

-- | Parse a bytestring into some kind of 'Serialize'.
newtype Get r = Get (StateT (ByteString, r) Maybe ())

instance Serialize Get where
  bytes i = Get $ get >>= \(bs, r) ->
    if B.length bs < i then empty else
      put (B.drop i bs, B.take i bs)
  Get b +> l = Get $ get >>= \(bs, cur) ->
    lift (execStateT b (bs, view l cur)) >>= \(bs, new) ->
      put (bs, set l new cur)
  Get a |+| Get b = Get $ a >> b

-- | Deserialize a value, given the value to start with.
deserialize' :: Get b -> b -> ByteString -> Maybe b
deserialize' (Get a) x bs = snd <$> execStateT a (bs, x)

-- | Deserialize a value, starting with the default value of its type.
deserialize :: Default b => Get b -> ByteString -> Maybe b
deserialize a bs = deserialize' a def bs
