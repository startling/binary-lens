module Control.Lens.Binary.Put.Strict where
-- mtl
import Control.Monad.Writer
import Control.Monad.Reader
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- lens
import Control.Lens
-- binary-lens
import Control.Lens.Binary.Serialize

-- | Build a bytestring from some 'Serialize'.
newtype Put r = Put (ReaderT r (Writer ByteString) ())

instance Serialize Put where
  bytes i = Put $ ask >>= tell . B.take i
  Put b +> l= Put $ withReaderT (view l) b
  Put a |+| Put b = Put $ a >> b

-- | Serialize some value, given a Put action.
serialize :: Put a -> a -> ByteString
serialize (Put a) = execWriter . runReaderT a
