module Poseidon.InterfaceSpec (spec) where

import           Poseidon.CLI.OptparseApplicativeParsers
import           Poseidon.GenotypeData                   (GenotypeFileSpec (..))

import qualified Options.Applicative                     as OP
import           Test.Hspec

spec :: Spec
spec = do
    testParseInGenoOne

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

