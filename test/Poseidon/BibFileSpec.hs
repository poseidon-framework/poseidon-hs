module Poseidon.BibFileSpec (spec) where

import           Poseidon.BibFile        (loadBibTeXFile, writeBibTeXFile)

import           Data.Text               (unpack)
import           Text.CSL.Reference      (unLiteral, Reference (..))
--import           Text.CSL.Style          (Agent (..))
import           Test.Hspec

spec = do
    testBibReadWriteReadCycle

testBibReadWriteReadCycle :: Spec
testBibReadWriteReadCycle = describe 
    "Poseidon.BibFile.loadBibTeXFile and Poseidon.BibFile.writeBibTeXFile" $ do
        let testBibFileIn = "test/testDat/testBibFiles/test.bib"
        let testBibFileOut = "/tmp/poseidonBibFileTest.bib"
        it "reading, writing and reading again should maintain (general) consistcency" $ do
            -- perform actions
            testReferences1 <- loadBibTeXFile testBibFileIn
            writeBibTeXFile testBibFileOut testReferences1
            testReferences2 <- loadBibTeXFile testBibFileOut
            -- test outcome
            map (unpack . unLiteral . refId) testReferences1 `shouldMatchList` ["A1971", "B2014", "P2020"]
            map (unpack . unLiteral . refId) testReferences1 `shouldMatchList` map (unpack . unLiteral . refId) testReferences2
            -- map (map familyName . author) testReferences1 `shouldMatchList` map (map familyName . author) testReferences2
            map title testReferences1 `shouldMatchList` map title testReferences2
            map doi testReferences1 `shouldMatchList` map doi testReferences2
            map eventDate testReferences1 `shouldMatchList` map eventDate testReferences2