module Chap1Spec (spec) where

import EduMci.Chap1.Main
import EduMci.Chap1.Prog1_5
import EduMci.Import.External
import Test.Hspec

spec :: Spec
spec = do
  it "maxargs" $ maxargs prog `shouldBe` 2
