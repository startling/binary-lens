{-# Language Rank2Types #-}
{-# Language ImplicitParams #-}
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
  => Serialize a -> Spec
testPutGet action = do
  it "can serialize and deserialize to get the same value" . property $
    \v -> deserialize action (serialize action v) == Just v
  it "can be deserialized from and serialize to the same bytestring"
    . property $ (B.pack <$> vectorOf (fromIntegral $ sizeOf action) arbitrary) <&>
      \bs -> (serialize action <$> deserialize action bs) == Just bs

-- | Check that something parses to the given thing.
shouldParseTo :: (?action :: Get b, Eq b, Show b, Default b)
  => ByteString -> b -> Expectation
a `shouldParseTo` b = deserialize ?action a `shouldBe` Just b

testByte :: Spec
testByte = describe "binary :: Serialize Word8" $ let
  ?action = binary :: Serialize Word8 in do
  it "parses single bytes" $ do
    B.singleton 255 `shouldParseTo` 255
    B.singleton 123 `shouldParseTo` 123
  testPutGet (binary :: Serialize Word8)

testWord16le :: Spec
testWord16le = describe "little :: Serialize Word16" $ let
  ?action = little :: Serialize Word16  in do
  it "parses pairs of bytes" $ do
    B.pack [0xff, 0xff] `shouldParseTo` 0xffff
  it "deals with endianity correctly" $ do
    B.pack [0xe8, 0x03] `shouldParseTo` 0x03e8
  testPutGet (little :: Serialize Word16)

testWord16be :: Spec
testWord16be = describe "big :: Serialize Word16" $ let
  ?action = big :: Serialize Word16 in do
  it "parses pairs of bytes" $ do
    B.pack [0xff, 0xff] `shouldParseTo` 0xffff
  it "deals with endianity correctly" $ do
    B.pack [0x03, 0xe8] `shouldParseTo` 0x03e8
  testPutGet (big :: Serialize Word16)

testWord32le :: Spec
testWord32le = describe "little :: Serialize Word32" $ let
  ?action = little :: Serialize Word32 in do
  it "parses four bytes" $ do
    B.pack [0xff, 0xff, 0xff, 0xff] `shouldParseTo` 0xffffffff
  it "deals with endianity correctly" $ do
    B.pack [0x03, 0xe8, 0x03, 0xe8] `shouldParseTo` 0xe803e803
  testPutGet (little :: Serialize Word32)

testWord32be :: Spec
testWord32be = describe "big :: Serialize Word32" $ let
  ?action = big :: Serialize Word32 in do
  it "parses four bytes" $ do
    B.pack [0xff, 0xff, 0xff, 0xff] `shouldParseTo` 0xffffffff
  it "deals with endianity correctly" $ do
    B.pack [0xe8, 0x03, 0xe8, 0x03] `shouldParseTo` 0xe803e803
  testPutGet (big :: Serialize Word32)

testWord64le :: Spec
testWord64le = describe "little :: Serialize Word64" $ let
  ?action = little :: Serialize Word64 in do
  it "parses eight bytes" $ do
    B.pack [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
      `shouldParseTo` 0xffffffffffffffff
  it "deals with endianity correctly" $ do
    B.pack [0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
      `shouldParseTo` 0xffeeddccbbaa9988
  testPutGet (little :: Serialize Word64)

testWord64be :: Spec
testWord64be = describe "big :: Serialize Word64" $ let
  ?action = big :: Serialize Word64 in do
  it "parses eight bytes" $ do
    B.pack [0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff]
      `shouldParseTo` 0xffffffffffffffff
  it "deals with endianity correctly" $ do
    B.pack [0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0xff]
      `shouldParseTo` 0x8899aabbccddeeff
  testPutGet (big :: Serialize Word64)

testTuples :: Spec
testTuples = do
  describe "binary :: Serialize (Word8, Word8)" $ do
    testPutGet (binary :: Serialize (Word8, Word8))
    it "has a size of 2" $ do
      sizeOf (binary :: Serialize (Word8, Word8))
        `shouldBe` 2
  describe "binary :: Serialize (Word8, Word8, Word8)" $ do
    testPutGet (binary :: Serialize (Word8, Word8, Word8))
    it "has a size of 3" $ do
      sizeOf (binary :: Serialize (Word8, Word8, Word8))
       `shouldBe` 3
  describe "binary :: Serialize (Word8, Word8, Word8, Word8)" $ do
    testPutGet (binary :: Serialize (Word8, Word8, Word8, Word8))
    it "has a size of 4" $ do
      sizeOf (binary :: Serialize (Word8, Word8, Word8, Word8))
       `shouldBe` 4
  describe "little :: Serialize (Word16, Word16)" $ do
    testPutGet (little :: Serialize (Word16, Word16))
    it "has a size of 4" $ do
      sizeOf (little :: Serialize (Word16, Word16))
        `shouldBe` 4
  describe "big :: Serialize (Word16, Word16)" $ do
    testPutGet (big :: Serialize (Word16, Word16))
    it "has a size of 4" $ do
      sizeOf (big :: Serialize (Word16, Word16))
        `shouldBe` 4
  describe "little :: Serialize (Word16, Word16, Word16)" $ do
    testPutGet (little :: Serialize (Word16, Word16, Word16))
    it "has a size of 6" $ do
      sizeOf (little :: Serialize (Word16, Word16, Word16))
       `shouldBe` 6
  describe "big :: Serialize (Word16, Word16, Word16)" $ do
    testPutGet (big :: Serialize (Word16, Word16, Word16))
    it "has a size of 6" $ do
      sizeOf (big :: Serialize (Word16, Word16, Word16))
       `shouldBe` 6
  describe "little :: Serialize (Word16, Word16, Word16, Word16)" $ do
    testPutGet (little :: Serialize (Word16, Word16, Word16, Word16))
    it "has a size of 8" $ do
      sizeOf (little :: Serialize (Word16, Word16, Word16, Word16))
       `shouldBe` 8
  describe "big :: Serialize (Word16, Word16, Word16, Word16)" $ do
    testPutGet (big :: Serialize (Word16, Word16, Word16, Word16))
    it "has a size of 8" $ do
      sizeOf (big :: Serialize (Word16, Word16, Word16, Word16))
       `shouldBe` 8

main :: IO ()
main = hspec $ do
  testByte
  testWord16le
  testWord16be
  testWord32le
  testWord32be
  testWord64le
  testWord64be
  testTuples
