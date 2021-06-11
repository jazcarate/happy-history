{-# LANGUAGE StandaloneDeriving #-}

module CursorSpec
  ( spec
  ) where

import           Cursor
import           RIO
import qualified RIO.Text                      as T
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

type Operation = TextCursor -> TextCursor

deriving instance Show TextCursor
deriving instance Eq TextCursor

operations :: Gen Operation
operations = elements [prev, next, home, end]

-- | Generates, from the same text, two cursors on different positions
twoCursors :: Gen (TextCursor, TextCursor)
twoCursors = do
  innitOps <- listOf operations
  txt      <- arbitrary
  let cur  = mkTextCursor txt
  let cur' = foldl' (&) cur innitOps
  pure (cur, cur')


spec :: Spec
spec = do
  describe "Text Cursor" $ do
    prop "roundtrip with no movement"
      $ \txt -> getText (mkTextCursor txt) `shouldBe` txt
    it "prev moves the cursor one char before" $ do
      let cur = prev $ prev $ mkTextCursor "foo"
      textCursorBefore cur `shouldBe` "f"
      textCursorCurrent cur `shouldBe` "o"
      textCursorAfter cur `shouldBe` "o "
    it "next moves the cursor one char after" $ do
      let cur = next $ prev $ prev $ mkTextCursor "foo"
      textCursorBefore cur `shouldBe` "fo"
      textCursorCurrent cur `shouldBe` "o"
      textCursorAfter cur `shouldBe` " "
    prop "prev from home does nothing " $ \txt ->
      (prev $ home $ mkTextCursor txt) `shouldBe` (home $ mkTextCursor txt)
    prop "next from end does nothing " $ \txt ->
      (next $ end $ mkTextCursor txt) `shouldBe` (end $ mkTextCursor txt)
    prop "going home does not care about previous movements"
      $ forAll twoCursors (\(ca, cb) -> home ca `shouldBe` home cb)
    prop "going end does not care about previous movements"
      $ forAll twoCursors (\(ca, cb) -> end ca `shouldBe` end cb)
    prop "getting text does not care about positioning"
      $ forAll twoCursors (\(ca, cb) -> getText ca `shouldBe` getText cb)
