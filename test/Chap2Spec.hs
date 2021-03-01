module Chap2Spec (spec) where

import qualified Data.ByteString.Char8 as BS
import EduMci.Chap2.Driver (scanner)
import EduMci.Import.External
import Test.Hspec
import TigerSrc (sources)

spec :: Spec
spec = do
  it "lex" $ mapM_ scan sources
  where
    scan (name, src) = do
      putStrLn $ "Lexing " <> name
      let res = scanner $ BS.unpack src
      print res
      res `shouldSatisfy` isRight
