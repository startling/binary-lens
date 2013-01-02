{-# Language Rank2Types #-}
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

main = hspec $ do
  describe "byteAt" $ do
    it "gets the zeroeth byte of a Word8" . property $
      \x -> (x ^. byteAt 0) == x
    it "sets the zeroeth byte of a Word8" . property $
      \x y -> (byteAt 0 .~ y $ (x :: Word8)) == y
    it "gets the zeroeth byte of a Word16" $ do
      (0xffff :: Word16) ^. byteAt 0 `shouldBe` 0xff
      (0xff00 :: Word16) ^. byteAt 0 `shouldBe` 0xff
      (0x00ff :: Word16) ^. byteAt 0 `shouldBe` 0x00
    it "sets the zeroeth byte of a Word16" $ do
      (byteAt 0 .~ 0xff $ 0x00ff :: Word16) `shouldBe` 0xffff
      (byteAt 0 .~ 0x00 $ 0xffff :: Word16) `shouldBe` 0x00ff
    it "gets the first byte of a Word16" $ do
      (0xffff :: Word16) ^. byteAt 1 `shouldBe` 0xff
      (0xff00 :: Word16) ^. byteAt 1 `shouldBe` 0x00
      (0x00ff :: Word16) ^. byteAt 1 `shouldBe` 0xff
    it "sets the first byte of a Word16" $ do
      (byteAt 1 .~ 0x00 $ 0x00ff :: Word16) `shouldBe` 0x0000
      (byteAt 1 .~ 0xff $ 0x0000 :: Word16) `shouldBe` 0x00ff
      (byteAt 1 .~ 0xff $ 0xff00 :: Word16) `shouldBe` 0xffff
  describe "byte" $ do
    testPutGet byte 1
  describe "word16le" $ do
    testPutGet word16le 2
  describe "word16be" $ do
    testPutGet word16be 2
  describe "word32le" $ do
    testPutGet word32le 4
  describe "word32be" $ do
    testPutGet word32be 4
  describe "word64le" $ do
    testPutGet word64le 8
  describe "word64be" $ do
    testPutGet word64be 8
