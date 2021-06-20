{-# LANGUAGE StandaloneDeriving #-}

module SearchSpec
  ( spec
  ) where

import           RIO
import           Search
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck


deriving instance Show a => Show (Match a)
deriving instance Eq a => Eq (Match a)

spec :: Spec
spec = do
  describe "search" $ do
    it "works as the example says" $ do
      let res = search "git" ["git add .", "goto it", "foo"]
      res `shouldBe` [[yes "git", no " add ."], [yes "g", no "oto ", yes "it"]]
    it "no match" $ (matchOn "git" "foo") `shouldBe` Nothing
    it "partial match" $ (matchOn "git" "gi") `shouldBe` Nothing
    it "full word match" $ (matchOn "git" "git") `shouldBe` Just [yes "git"]
    it "match and more" $ (matchOn "git" "gitx") `shouldBe` Just
      [yes "git", no "x"]
    it "match and les?" $ (matchOn "git" "xgit") `shouldBe` Just
      [no "x", yes "git"]
    it "hole" $ (matchOn "git" "gxit") `shouldBe` Just
      [yes "g", no "x", yes "it"]
    it "hole compact" $ (matchOn "git" "gxxxit") `shouldBe` Just
      [yes "g", no "xxx", yes "it"]
