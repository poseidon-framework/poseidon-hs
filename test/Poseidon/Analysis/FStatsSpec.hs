{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Poseidon.Analysis.FStatsSpec (spec) where


import           Poseidon.Analysis.CLI.FStats   (collectStatSpecGroups)
import           Poseidon.Analysis.FStatsConfig (AscertainmentSpec (..),
                                                 FStatSpec (..), FStatType (..),
                                                 fStatSpecParser,
                                                 readFStatsSimpleText)
import           Poseidon.EntityTypes           (PoseidonEntity (..))

import qualified Data.ByteString.Char8          as B
import           System.IO                      (IOMode (..), withFile)
import           Test.Hspec
import           Text.Parsec                    (runParser)
import           Text.RawString.QQ

spec :: Spec
spec = do
    testCollectStats
    testParseStatSpec
    testParseStatSpecFromFile

testParseStatSpec :: Spec
testParseStatSpec = describe "Poseidon.FStatsSpec.fStatSpecParser" $ do
    it "should parse F4 correctly" $
        let f = FStatSpec F4 [Group "aa", Ind "bb", Group "cc", Group "dd"] Nothing
        in  runParser fStatSpecParser () "" "F4(aa,<bb>,cc,dd)" `shouldBe` Right f
    it "should parse F3 correctly" $
        let f = FStatSpec F3 [Group "aa", Ind "bb", Group "cc"] Nothing
        in  runParser fStatSpecParser () "" "F3(aa,<bb>,cc)" `shouldBe` Right f
    it "should parse F2 correctly" $
        let f = FStatSpec F2 [Group "aa", Ind "bb"] Nothing
        in  runParser fStatSpecParser () "" "F2(aa,<bb>)" `shouldBe` Right f
    it "should parse PWM correctly" $
        let f = FStatSpec PWM [Group "aa", Ind "bb"] Nothing
        in  runParser fStatSpecParser () "" "PWM(aa,<bb>)" `shouldBe` Right f
    it "should not parse wrong header" $
        show (runParser fStatSpecParser () "" "BlaBla(aa,<bb>)") `shouldBe` "Left (line 1, column 7):\nunexpected \"(\"\nCannot parse Statistic type BlaBla. Must be one of F4, F3, F2, FST, PWM, Het, F3vanilla, FSTvanilla"
    it "should not parse too few args" $
        show (runParser fStatSpecParser () "" "F4(aa,<bb>,cc)") `shouldBe` "Left (line 1, column 15):\nNot the right number of arguments to Statistic F4"
    it "should not parse too many args" $
        show (runParser fStatSpecParser () "" "F4(aa,<bb>,cc,dd,ee)") `shouldBe` "Left (line 1, column 21):\nNot the right number of arguments to Statistic F4"

fStatTestSpec :: B.ByteString
fStatTestSpec = [r|
F4(English, French, Mbuti, Saami)

F3(English, French, <German1>)
PWM(English, <French1>)|]

testParseStatSpecFromFile :: Spec
testParseStatSpecFromFile = describe "Poseidon.FStatsSpec.readFStatsSimpleText" $ do
    let fn = "/tmp/fstats_test.txt"
    it "should parse a set of Fstats correctly from a file" $ do
        withFile fn WriteMode $ \h -> B.hPutStr h fStatTestSpec
        stats <- readFStatsSimpleText fn
        stats `shouldBe` [
            FStatSpec F4 [Group "English", Group "French", Group "Mbuti", Group "Saami"] Nothing,
            FStatSpec F3 [Group "English", Group "French", Ind "German1"] Nothing,
            FStatSpec PWM [Group "English", Ind "French1"] Nothing]


testCollectStats :: Spec
testCollectStats = describe "collectStatSpecGroups" $ do
    it "should correctly collect stats" $ do
        let statSpecs = [
                FStatSpec F3star [Group "French", Group "Spanish", Ind "Chimp.REF"] Nothing,
                FStatSpec F3star [Group "French", Group "Mbuti", Ind "Chimp.REF"] (Just (AscertainmentSpec (Just (Ind "Human.REF")) (Group "CEU") 0.05 0.95))]
            entities = [Group "French", Group "Spanish", Ind "Chimp.REF", Group "Mbuti", Ind "Human.REF", Group "CEU"]
        collectStatSpecGroups statSpecs `shouldMatchList` entities
