module TypesSpec (spec) where

import RIO
import Haskip8.Types

import Test.Hspec
--import Test.Hspec.QuickCheck


spec :: Spec
spec = --do
  describe "C8Addr" $ do
    it "support addition" $ (c8addr 0x200 + 2) `shouldBe` c8addr 0x202
    it "overflow" $ ((maxBound :: C8Addr) + 2) `shouldBe` ((minBound :: C8Addr) + 1)
    it "maxBound is 0xfff" $ (maxBound :: C8Addr) `shouldBe` c8addr 0xfff
