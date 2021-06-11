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

instance Arbitrary Operation where
  arbitrary =
    elements
      $ [ Operation "prev" prev
        , Operation "next" next
        , Operation "home" home
        , Operation "end"  end
        ]

instance Arbitrary TextCursor where
  arbitrary = do
    txt <- arbitrary
    let cur = mkTextCursor txt
    ops <- arbitrary
    pure $ foldOps cur ops

foldOps :: TextCursor -> [Operation] -> TextCursor
foldOps cur ops = foldl' (&) cur (run <$> ops)

data Operation = Operation
  { name :: String
  , run  :: TextCursor -> TextCursor
  }

instance Show Operation where
  show = name

deriving instance Show TextCursor
deriving instance Eq TextCursor


spec :: Spec
spec = do
  describe "Text Cursor" $ do
    prop "roundtrip with no movement"
      $ \txt -> getText (mkTextCursor txt) `shouldBe` txt
    it "can delete current pointed"
      $          (getText $ delete $ mkTextCursor "foo")
      `shouldBe` "fo"
    it "can add to current pointed"
      $          (getText $ append "b" $ mkTextCursor "foo")
      `shouldBe` "foob"
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
    prop "home is the same as multiple prevs" $ \txt ->
      (home $ mkTextCursor txt)
        `shouldBe` (foldl' (&)
                           (mkTextCursor txt)
                           (replicate (T.length txt) prev)
                   )
    prop "end is the same as multiple nexts" $ \txt ->
      (end $ mkTextCursor txt)
        `shouldBe` (foldl' (&)
                           (mkTextCursor txt)
                           (replicate (T.length txt) next)
                   )
    prop "prev from home does nothing "
      $ \cur -> (prev $ home $ cur) `shouldBe` (home cur)
    prop "next from end does nothing "
      $ \cur -> (next $ end $ cur) `shouldBe` (end $ cur)
    prop "going home does not care about previous movements"
      $ \(cur, ops) -> home cur `shouldBe` (home $ foldOps cur ops)
    prop "going end does not care about previous movements"
      $ \(cur, ops) -> end cur `shouldBe` (end $ foldOps cur ops)
    prop "getting text does not care about positioning"
      $ \(cur, ops) -> getText cur `shouldBe` (getText $ foldOps cur ops)
    it "can move one token at a time (prev)"
      $          (prevToken $ mkTextCursor "foo bar")
      `shouldBe` (prev $ prev $ prev $ mkTextCursor "foo bar")
    it "can move one token at a time (next)"
      $          (nextToken $ home $ mkTextCursor "foo bar")
      `shouldBe` (next $ next $ next $ home $ mkTextCursor "foo bar")
