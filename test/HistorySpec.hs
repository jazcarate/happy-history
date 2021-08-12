{-# LANGUAGE StandaloneDeriving #-}

module HistorySpec
  ( spec
  ) where

import           History
import           RIO
import           Test.Hspec



deriving instance Show PastCommand
deriving instance Eq PastCommand

spec :: Spec
spec = do
  describe "parse" $ do
    it "can parse a normal line"
      $          (pastCommand " 1904  stack test")
      `shouldBe` (Just $ PastCommand "stack test")
    it "fails to parse a line without history index"
      $          (pastCommand "stack test")
      `shouldBe` Nothing
    it "fails to parse a line without propped formatting"
      $          (pastCommand "123-stack test")
      `shouldBe` Nothing
    it "can parse any number of leading spaces"
      $          (pastCommand "   1  stack test")
      `shouldBe` (Just $ PastCommand "stack test")
