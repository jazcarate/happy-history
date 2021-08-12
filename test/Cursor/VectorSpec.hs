{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.VectorSpec
  ( spec
  ) where

import           Cursor.Vector
import           RIO
import qualified RIO.Vector                    as V
import           Test.Hspec
import           Test.QuickCheck

deriving instance Eq a => Eq (Cursor a)
deriving instance Show a => Show (Cursor a)

type SomeType = String

emptyCursor :: Cursor SomeType
emptyCursor = mkCursor mempty

exampleCursor :: Cursor SomeType
exampleCursor = mkCursor $ V.fromList ["foo", "bar"]

instance Arbitrary a => Arbitrary (Cursor a) where
  arbitrary = mkCursor . V.fromList <$> arbitrary

spec :: Spec
spec = do
  describe "Vector Cursor" $ do
    it "an empty vector has nothing selected"
      $          (getSelected emptyCursor)
      `shouldBe` Nothing
    it "a vector with something defaults to the first item selected"
      $          (getSelected exampleCursor)
      `shouldBe` Just "foo"
    describe "next" $ do
      it "an empty vector is idempotent"
        $          next emptyCursor
        `shouldBe` emptyCursor
      it "move to the next selection"
        $          (getSelected $ next exampleCursor)
        `shouldBe` Just "bar"
      it "does not overflow"
        $          (getSelected $ next $ next $ exampleCursor)
        `shouldBe` Just "bar"
    describe "prev" $ do
      it "an empty vector is idempotent"
        $          prev emptyCursor
        `shouldBe` emptyCursor
      it "does not underflow"
        $          (getSelected $ prev exampleCursor)
        `shouldBe` Just "foo"
{-     prop "next and prev cancel out" $ \(cursor :: Cursor SomeType) ->
      (next $ prev $ cursor) `shouldBe` cursor -}
