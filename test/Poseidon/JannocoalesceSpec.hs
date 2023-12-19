{-# LANGUAGE OverloadedStrings #-}
module Poseidon.JannocoalesceSpec (spec) where

import           Poseidon.CLI.Jannocoalesce (makeNewJannoRows, mergeRow)
import           Poseidon.Janno             (CsvNamedRecord (..),
                                             JannoList (..), JannoRow (..),
                                             createMinimalSample, makeLatitude,
                                             makeLongitude)

import qualified Data.HashMap.Strict        as HM
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..), Sex (..))
import           Test.Hspec

spec :: Spec
spec = do
    testMergeSingleRow
    testCoalesceMultipleRows

jannoTargetRow :: JannoRow
jannoTargetRow =
    let row = createMinimalSample (EigenstratIndEntry "Name" Male "SamplePop")
    in  row {
            jCountry = Just "Austria",
            jSite = Just "Vienna",
            jDateNote = Just "dating didn't work"
        }

jannoSourceRow :: JannoRow
jannoSourceRow =
    let row = createMinimalSample (EigenstratIndEntry "Name" Male "SamplePop2")
    in  row {
            jCountry   = Just "Austria",
            jSite      = Just "Salzburg",
            jLatitude  = makeLatitude 30.0,
            jLongitude = makeLongitude 30.0
        }

jannoTargetRows :: [JannoRow]
jannoTargetRows =
    let row1 = createMinimalSample (EigenstratIndEntry "Ind1" Male "SamplePop")
        row2 = createMinimalSample (EigenstratIndEntry "Ind2" Male "SamplePop")
        row3 = createMinimalSample (EigenstratIndEntry "Ind3_AB" Male "SamplePop")
    in  [row1 {jCountry = Just "Germany"}, row2, row3]

jannoSourceRows :: [JannoRow]
jannoSourceRows =
    let row1 = createMinimalSample (EigenstratIndEntry "Ind1" Male "SamplePop")
        row2 = createMinimalSample (EigenstratIndEntry "Ind2" Male "SamplePop")
        row3 = createMinimalSample (EigenstratIndEntry "Ind3" Male "SamplePop")
    in  [
            row1 {jCountry = Just "Austria", jAdditionalColumns = CsvNamedRecord $ HM.fromList [("Poseidon_ID_alt", "Ind1")]},
            row2 {jCountry = Just "Austria", jAdditionalColumns = CsvNamedRecord $ HM.fromList [("Poseidon_ID_alt", "Ind2")]},
            row3 {jCountry = Just "Austria", jAdditionalColumns = CsvNamedRecord $ HM.fromList [("Poseidon_ID_alt", "Ind3_AB")]}]

testMergeSingleRow :: Spec
testMergeSingleRow =
    describe "Poseidon.Jannocoalesce.mergeRow" $ do
        it "should correctly merge without fields and no override" $ do
            merged <- mergeRow jannoTargetRow jannoSourceRow [] False "Poseidon_ID"
            jSite merged      `shouldBe` Just "Vienna"
            jGroupName merged `shouldBe` JannoList ["SamplePop"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` makeLongitude 30.0
        it "should correctly merge without fields and override" $ do
            merged <- mergeRow jannoTargetRow jannoSourceRow [] True "Poseidon_ID"
            jSite merged      `shouldBe` Just "Salzburg"
            jGroupName merged `shouldBe` JannoList ["SamplePop2"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` makeLongitude 30.0
        it "should correctly merge with fields and no override" $ do
            merged <- mergeRow jannoTargetRow jannoSourceRow ["Group_Name", "Latitude"] False "Poseidon_ID"
            jSite merged      `shouldBe` Just "Vienna"
            jGroupName merged `shouldBe` JannoList ["SamplePop"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` Nothing
        it "should correctly merge with fields and override" $ do
            merged <- mergeRow jannoTargetRow jannoSourceRow ["Group_Name", "Latitude"] True "Poseidon_ID"
            jSite merged      `shouldBe` Just "Vienna"
            jGroupName merged `shouldBe` JannoList ["SamplePop2"]
            jLatitude merged  `shouldBe` makeLatitude 30.0
            jLongitude merged `shouldBe` Nothing

testCoalesceMultipleRows :: Spec
testCoalesceMultipleRows = describe "Poseidon.Jannocoalesce.makeNewJannoRows" $ do
    it "should correctly copy with simple matching" $ do
        newJ <- makeNewJannoRows jannoSourceRows jannoTargetRows [] False "Poseidon_ID" "Poseidon_ID" Nothing
        jCountry (newJ !! 0) `shouldBe` Just "Germany"
        jCountry (newJ !! 1) `shouldBe` Just "Austria"
        jCountry (newJ !! 2) `shouldBe` Nothing
    it "should correctly copy with simple matching and overwrite" $ do
        newJ <- makeNewJannoRows jannoSourceRows jannoTargetRows [] True "Poseidon_ID" "Poseidon_ID" Nothing
        jCountry (newJ !! 0) `shouldBe` Just "Austria"
        jCountry (newJ !! 1) `shouldBe` Just "Austria"
    it "should throw with duplicate source keys" $ do
        let s = jannoSourceRows ++ [jannoSourceRows !! 1]
        makeNewJannoRows s jannoTargetRows [] False "Poseidon_ID" "Poseidon_ID" Nothing `shouldThrow` anyException
    it "should correctly copy with suffix strip" $ do
        newJ <- makeNewJannoRows jannoSourceRows jannoTargetRows [] False "Poseidon_ID" "Poseidon_ID" (Just "_AB")
        jCountry (newJ !! 0) `shouldBe` Just "Germany"
        jCountry (newJ !! 1) `shouldBe` Just "Austria"
        jCountry (newJ !! 2) `shouldBe` Just "Austria"
    it "should correctly copy with alternative ID column" $ do
        newJ <- makeNewJannoRows jannoSourceRows jannoTargetRows [] False "Poseidon_ID_alt" "Poseidon_ID" Nothing
        jCountry (newJ !! 2) `shouldBe` Just "Austria"
