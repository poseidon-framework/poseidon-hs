module Poseidon.InterfaceSpec (spec) where

import           Poseidon.CLI.OptparseApplicativeParsers
import           Poseidon.GenotypeData                   (GenotypeFileSpec (..))

import qualified Options.Applicative                     as OP
import           Test.Hspec

spec :: Spec
spec = do
    testParseInGenoOne
    testParseInGenoSep
    testSplitExtensionsOptGz

runParser :: OP.Parser a -> [String] -> Maybe a
runParser p s = OP.getParseResult $ OP.execParserPure OP.defaultPrefs (OP.info p mempty) s

testParseInGenoOne :: Spec
testParseInGenoOne = describe
    "Poseidon.OptparseApplicativeParsers.parseInGenoOne" $ do
        it "should detect zipped files correctly" $ do
            let maybeValEigenstrat = runParser parseInGenoOne ["-p", "path/to/file.geno.gz"]
            maybeValEigenstrat `shouldBe`
                Just (GenotypeEigenstrat "path/to/file.geno.gz" Nothing
                                         "path/to/file.snp.gz"  Nothing
                                         "path/to/file.ind"     Nothing)
            let maybeValPlink = runParser parseInGenoOne ["-p", "path/to/file.bim.gz"]
            maybeValPlink `shouldBe`
                Just (GenotypePlink "path/to/file.bed.gz" Nothing
                                    "path/to/file.bim.gz" Nothing
                                    "path/to/file.fam"    Nothing)

testParseInGenoSep :: Spec
testParseInGenoSep = describe
    "Poseidon.OptparseApplicativeParsers.parseInGenoSep" $ do
        it "should return the expected paths for EIGENSTRAT data" $ do
            runParser parseInGenoSep [
                  "--genoFile", "path/to/file.test.geno.gz"
                , "--snpFile",  "path/to/file.snp"
                , "--indFile",  "path/to/file.ind"
                ] `shouldBe`
                Just (GenotypeEigenstrat "path/to/file.test.geno.gz" Nothing
                                         "path/to/file.snp"          Nothing
                                         "path/to/file.ind"          Nothing)

testSplitExtensionsOptGz :: Spec
testSplitExtensionsOptGz = describe
    "Poseidon.OptparseApplicativeParsers.testSplitExtensionsOptGz" $ do
        it "should split withempty ending" $
            splitExtensionsOptGz "myFile_noEnding" `shouldBe` ("myFile_noEnding", "")
        it "should return an single extension if not gz" $
            splitExtensionsOptGz "myFile.txt" `shouldBe` ("myFile", ".txt")
        it "...even if there are more dots" $
            splitExtensionsOptGz "myFile.double.txt" `shouldBe` ("myFile.double", ".txt")
        it "should return only gz if that's the only ending" $
            splitExtensionsOptGz "myFile.gz" `shouldBe` ("myFile", ".gz")
        it "should return two endings if ending with gz" $
            splitExtensionsOptGz "myFile.txt.gz" `shouldBe` ("myFile", ".txt.gz")
        it "even if there are more" $
            splitExtensionsOptGz "myFile.double.txt.gz" `shouldBe` ("myFile.double", ".txt.gz")
