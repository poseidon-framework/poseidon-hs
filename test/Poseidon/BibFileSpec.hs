module Poseidon.BibFileSpec (spec) where

import           Poseidon.BibFile (BibEntry (..), authorAbbrvString,
                                   parseAuthors, readBibTeXFile,
                                   writeBibTeXFile)

import           Test.Hspec

spec :: Spec
spec = do
    testBibReadWriteReadCycle
    testParseAuthors
    testAuthorAbbrvString

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

testParseAuthors :: Spec
testParseAuthors = describe "Poseidon.BibFile.parseAuthors" $ do
    let authorStr = "Lazaridis, Iosif and Patterson, Nick and Mittnik, Alissa and Lamnidis, Thiseas Christos"
    it "should parse authors correctly" $
        parseAuthors authorStr `shouldReturn` [("Iosif", "Lazaridis"), ("Nick", "Patterson"), ("Alissa", "Mittnik"), ("Thiseas Christos", "Lamnidis")]

testAuthorAbbrvString :: Spec
testAuthorAbbrvString = describe "Poseidon.BibFile.authorAbbrvString" $ do
    it "should correctly render single author" $
        authorAbbrvString [("Susie", "Haak")] `shouldBe` "Susie Haak"
    it "should correctly render two authors" $
        authorAbbrvString [("Susie", "Haak"), ("Jack", "Ryan")] `shouldBe` "S Haak and J Ryan"
    it "should correctly render more than two authors" $
        authorAbbrvString [("Susie", "Haak"), ("Jack", "Ryan"), ("Sabrina", "Fisher")] `shouldBe` "S Haak et al."


