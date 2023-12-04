{-# LANGUAGE OverloadedStrings #-}
module Poseidon.JannocoalesceSpec (spec) where

import           Poseidon.CLI.Jannocoalesce (mergeRow)
import           Poseidon.Janno             (JannoList (..), JannoRow (..),
                                             createMinimalSample,
                                             makeLatitude, makeLongitude)

import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..), Sex (..))
import           Test.Hspec

jannoTarget :: JannoRow
jannoTarget =
    let row = createMinimalSample (EigenstratIndEntry "Name" Male "SamplePop")
    in  row {
            jCountry = Just "Austria",
            jSite = Just "Vienna",
            jDateNote = Just "dating didn't work"
        }

jannoSource :: JannoRow
jannoSource =
    let row = createMinimalSample (EigenstratIndEntry "Name" Male "SamplePop2")
    in  row {
            jCountry   = Just "Austria",
            jSite      = Just "Salzburg",
            jLatitude  = makeLatitude 30.0,
            jLongitude = makeLongitude 30.0
        }

spec :: Spec
spec = do
    testMergeRow

testMergeRow :: Spec
testMergeRow =
    describe "Poseidon.Jannocoalesce.mergeRow" $ do
        it "should correctly merge without fields and no override" $ do
            merged <- mergeRow jannoTarget jannoSource [] False
            jSite merged      `shouldBe` Just "Vienna"
            jGroupName merged `shouldBe` JannoList ["SamplePop"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` makeLongitude 30.0
        it "should correctly merge without fields and override" $ do
            merged <- mergeRow jannoTarget jannoSource [] True
            jSite merged      `shouldBe` Just "Salzburg"
            jGroupName merged `shouldBe` JannoList ["SamplePop2"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` makeLongitude 30.0
        it "should correctly merge with fields and no override" $ do
            merged <- mergeRow jannoTarget jannoSource ["Group_Name", "Latitude"] False
            jSite merged      `shouldBe` Just "Vienna"
            jGroupName merged `shouldBe` JannoList ["SamplePop"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` Nothing
        it "should correctly merge with fields and override" $ do
            merged <- mergeRow jannoTarget jannoSource ["Group_Name", "Latitude"] True
            jSite merged      `shouldBe` Just "Vienna"
            jGroupName merged `shouldBe` JannoList ["SamplePop2"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` Nothing

