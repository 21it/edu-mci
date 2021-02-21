module Chap1Spec (spec) where

import qualified Data.Map as Map
import EduMci.Chap1.Main
import EduMci.Chap1.Prog1_5
import EduMci.Import.External
import Test.Hspec

spec :: Spec
spec = do
  it "maxargs" $ maxargs prog `shouldBe` 2
  it "interp" $ interp prog `shouldBe` st
  where
    st =
      St
        { stTable = Map.fromList [("a", 8), ("b", 80)],
          stStdout = Stdout [Line [8, 7], Line [80]]
        }
