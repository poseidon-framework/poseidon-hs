module Poseidon.BibFileSpec (spec) where

import           Poseidon.BibFile (BibEntry (..), readBibTeXFile,
                                   writeBibTeXFile)

import           Test.Hspec

spec :: Spec
spec = do
    testBibReadWriteReadCycle

testBibReadWriteReadCycle :: Spec
testBibReadWriteReadCycle = describe
    "Poseidon.BibFile.readBibTeXFile and Poseidon.BibFile.writeBibTeXFile" $ do
        let testBibFileIn = "test/testDat/testBibFiles/test.bib"
        let testBibFileOut = "/tmp/poseidonBibFileTest.bib"
        it "reading, writing and reading again should maintain (general) consistcency" $ do
            -- perform actions
            testReferences1 <- readBibTeXFile testBibFileIn
            writeBibTeXFile testBibFileOut testReferences1
            testReferences2 <- readBibTeXFile testBibFileOut

            -- test outcome
            map bibEntryId testReferences1 `shouldMatchList` ["A1971", "B2014", "P2020"]
            map bibEntryId testReferences1 `shouldMatchList` map bibEntryId testReferences2
            map bibEntryFields testReferences1 `shouldMatchList` map bibEntryFields testReferences2
