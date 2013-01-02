{-# Language Rank2Types #-}
module Main where
-- base
import Data.Word
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
  => (forall s. Serialize s => s a) -> Spec
testPutGet action = 
  it "can serialize and deserialize to get the same value" . property $
    \v -> deserialize action (serialize action v) == Just v

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
  describe "byte" $ do
    testPutGet byte
  describe "word16le" $ do
    testPutGet word16le
  describe "word16be" $ do
    testPutGet word16be
  describe "word32le" $ do
    testPutGet word32le
  describe "word32be" $ do
    testPutGet word32be
