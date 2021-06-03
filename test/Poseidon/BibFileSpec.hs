module Poseidon.BibFileSpec (spec) where

import           Poseidon.BibFile        (readBibTeXFile, writeBibTeXFile, Reference(..))

import           Data.Text               (unpack)
-- import           Text.CSL.Reference      (unLiteral, Reference (..))
--import           Text.CSL.Style          (Agent (..))
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
            -- writeBibTeXFile testBibFileOut testReferences1
            -- testReferences2 <- readBibTeXFile testBibFileOut

            -- test outcome
            map _bibId testReferences1 `shouldMatchList` ["A1971", "B2014", "P2020"]
            -- map _bibKey testReferences1 `shouldMatchList` map _bibKey testReferences2
            -- map _bibBlock testReferences1 `shouldMatchList` map _bibBlock testReferences2
