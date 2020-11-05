{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Poseidon.FStatsSpec (spec) where

import           Poseidon.CmdFStats (FStatSpec (..), PopSpec (..), fStatSpecParser,
                                  runParser, ParseError(..), readStatSpecsFromFile)

import qualified Data.ByteString.Char8     as B
import           System.IO (withFile, IOMode(..))
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testParseStatSpec
    testParseStatSpecFromFile

testParseStatSpec :: Spec
testParseStatSpec = describe "Poseidon.FStatsSpec.fStatSpecParser" $ do
    it "should parse F4 correctly" $
        let f = F4Spec (PopSpecGroup "aa") (PopSpecInd "bb") (PopSpecGroup "cc") (PopSpecGroup "dd")
        in  runParser fStatSpecParser () "" "F4(aa,<bb>,cc,dd)" `shouldBe` Right f
    it "should parse F3 correctly" $
        let f = F3Spec (PopSpecGroup "aa") (PopSpecInd "bb") (PopSpecGroup "cc")
        in  runParser fStatSpecParser () "" "F3(aa,<bb>,cc)" `shouldBe` Right f
    it "should parse F2 correctly" $
        let f = F2Spec (PopSpecGroup "aa") (PopSpecInd "bb")
        in  runParser fStatSpecParser () "" "F2(aa,<bb>)" `shouldBe` Right f
    it "should parse PWM correctly" $
        let f = PWMspec (PopSpecGroup "aa") (PopSpecInd "bb")
        in  runParser fStatSpecParser () "" "PWM(aa,<bb>)" `shouldBe` Right f
    it "should not parse wrong header" $
        show (runParser fStatSpecParser () "" "BlaBla(aa,<bb>)") `shouldBe` "Left (line 1, column 1):\nunexpected \"B\"\nexpecting \"F4\", \"F3\", \"F2\" or \"PWM\""
    it "should not parse too few args" $
        show (runParser fStatSpecParser () "" "F4(aa,<bb>,cc)") `shouldBe` "Left (line 1, column 14):\nunexpected \")\"\nexpecting \",\""
    it "should not parse too many args" $
        show (runParser fStatSpecParser () "" "F4(aa,<bb>,cc,dd,ee)") `shouldBe` "Left (line 1, column 17):\nunexpected \",\"\nexpecting \")\""

fStatTestSpec :: B.ByteString
fStatTestSpec = [r|
F4(English, French, Mbuti, Saami)

F3(English, French, <German1>)
PWM(English, <French1>)|]

testParseStatSpecFromFile :: Spec
testParseStatSpecFromFile = describe "Poseidon.FStatsSpec.readStatSpecsFromFile" $ do
    let fn = "/tmp/fstats_test.txt"
    it "should parse a set of Fstats correctly from a file" $ do
        withFile fn WriteMode $ \h -> B.hPutStr h fStatTestSpec
        stats <- readStatSpecsFromFile fn
        stats `shouldBe` [
            F4Spec (PopSpecGroup "English") (PopSpecGroup "French") (PopSpecGroup "Mbuti") (PopSpecGroup "Saami"),
            F3Spec (PopSpecGroup "English") (PopSpecGroup "French") (PopSpecInd "German1"),
            PWMspec (PopSpecGroup "English") (PopSpecInd "French1")]

