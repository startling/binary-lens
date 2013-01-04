{-# Language Rank2Types #-}
{-# Language ImplicitParams #-}
{-# Language NoMonomorphismRestriction #-}
module Main where
-- base
import Data.Word
import Control.Applicative
-- bytestring
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-- lens
import Control.Lens
-- data-default
import Data.Default
-- binary-lens
import Control.Lens.Binary
-- QuickCheck
import Test.QuickCheck
-- hspec
import Test.Hspec

testPutGet :: (Arbitrary a, Default a, Eq a, Show a)
  => (forall s. Serialize s => s a) -> Int -> Spec
testPutGet action size = do
  it "can serialize and deserialize to get the same value" . property $
    \v -> deserialize action (serialize action v) == Just v
  it "can be deserialized from and serialize to the same bytestring"
    . property $ (B.pack <$> vectorOf size arbitrary) <&>
      \bs -> (serialize action <$> deserialize action bs) == Just bs

-- | Check that something parses to the given thing.
shouldParseTo :: (?action :: Get b, Eq b, Show b, Default b)
  => ByteString -> b -> Expectation
a `shouldParseTo` b = deserialize ?action a `shouldBe` Just b

main = hspec $ do
  describe "byte" $ let ?action = byte in do
    it "parses single bytes" $ do
      B.singleton 255 `shouldParseTo` 255
      B.singleton 123 `shouldParseTo` 123
    testPutGet byte 1
  describe "word16le" $ let ?action = word16le in do
    it "parses pairs of bytes" $ do
      B.pack [0xff, 0xff] `shouldParseTo` 0xffff
    it "deals with endianity correctly" $ do
      B.pack [0xe8, 0x03] `shouldParseTo` 0x03e8
    testPutGet word16le 2
  describe "word16be" $ let ?action = word16be in do
    it "parses pairs of bytes" $ do
      B.pack [0xff, 0xff] `shouldParseTo` 0xffff
    it "deals with endianity correctly" $ do
      B.pack [0x03, 0xe8] `shouldParseTo` 0x03e8
    testPutGet word16be 2
  describe "word32le" $ do
    testPutGet word32le 4
  describe "word32be" $ do
    testPutGet word32be 4
  describe "word64le" $ do
    testPutGet word64le 8
  describe "word64be" $ do
    testPutGet word64be 8
