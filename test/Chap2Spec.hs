module Chap2Spec (spec) where

import qualified Data.ByteString.Char8 as BS
import EduMci.Chap2.Driver (scanner)
import EduMci.Chap2.Lexer (AlexPosn (..), Lexeme (..))
import EduMci.Import.External
import Test.Hspec
import TigerSrc (sources)

spec :: Spec
spec = do
  it "lex" $ mapM_ scan sources
  where
    noPosn = (((\x -> x {lexPosn = AlexPn 0 0 0}) <$>) <$>)
    scan (_, src0) = do
      let res0 = scanner $ BS.unpack src0
      res0 `shouldSatisfy` isRight
      let src1 =
            intercalate " " $
              fromMaybe mempty . lexRaw
                <$> fromRight mempty res0
      let res1 = scanner src1
      res1 `shouldSatisfy` isRight
      noPosn res1 `shouldBe` noPosn res0
