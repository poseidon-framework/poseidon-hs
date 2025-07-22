{-# LANGUAGE OverloadedStrings #-}

module Poseidon.JannocoalesceSpec (spec) where

import           Poseidon.CLI.Jannocoalesce (CoalesceJannoColumnSpec (..),
                                             makeNewJannoRows, mergeRow)
import           Poseidon.ColumnTypesJanno
import           Poseidon.ColumnTypesUtils
import           Poseidon.Janno             (JannoRow (..),
                                             createMinimalSample)
import           Poseidon.Utils             (testLog)

import           Control.Monad.IO.Class     (liftIO)
import qualified Data.HashMap.Strict        as HM
import qualified Data.IORef                 as R
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
            jCountry = Just $ JannoCountry "Austria",
            jSite = Just $ JannoSite "Vienna",
            jDateNote = Just $ JannoDateNote "dating didn't work",
            jAdditionalColumns = CsvNamedRecord $ HM.fromList [
                ("AdditionalColumn2", "C")
            ]
        }

jannoSourceRow :: JannoRow
jannoSourceRow =
    let row = createMinimalSample (EigenstratIndEntry "Name" Male "SamplePop2")
    in  row {
            jCountry   = Just $ JannoCountry "Austria",
            jSite      = Just $ JannoSite "Salzburg",
            jLatitude  = make "30.0",
            jLongitude = make "30.0",
            jAdditionalColumns = CsvNamedRecord $ HM.fromList [
                ("AdditionalColumn1", "A"),
                ("AdditionalColumn2", "B")
            ]
        }

jannoTargetRows :: [JannoRow]
jannoTargetRows =
    let row1 = createMinimalSample (EigenstratIndEntry "Ind1" Male "SamplePop")
        row2 = createMinimalSample (EigenstratIndEntry "Ind2" Male "SamplePop")
        row3 = createMinimalSample (EigenstratIndEntry "Ind3_AB" Male "SamplePop")
    in  [row1 {jCountry = Just $ JannoCountry "Germany"}, row2, row3]

jannoSourceRows :: [JannoRow]
jannoSourceRows =
    let row1 = createMinimalSample (EigenstratIndEntry "Ind1" Male "SamplePop")
        row2 = createMinimalSample (EigenstratIndEntry "Ind2" Male "SamplePop")
        row3 = createMinimalSample (EigenstratIndEntry "Ind3" Male "SamplePop")
    in  [
            row1 {jCountry = Just $ JannoCountry "Austria", jAdditionalColumns = CsvNamedRecord $ HM.fromList [("Poseidon_ID_alt", "Ind1")]},
            row2 {jCountry = Just $ JannoCountry "Austria", jAdditionalColumns = CsvNamedRecord $ HM.fromList [("Poseidon_ID_alt", "Ind2")]},
            row3 {jCountry = Just $ JannoCountry "Austria", jAdditionalColumns = CsvNamedRecord $ HM.fromList [("Poseidon_ID_alt", "Ind3_AB")]}]

testMergeSingleRow :: Spec
testMergeSingleRow =
    describe "Poseidon.Jannocoalesce.mergeRow" $ do
        it "should correctly merge without fields and no override" $ do
            cp <- liftIO $ R.newIORef 0
            merged <- testLog $ mergeRow cp jannoTargetRow jannoSourceRow AllJannoColumns False "Poseidon_ID" "Poseidon_ID"
            jSite merged      `shouldBe` Just (JannoSite "Vienna")
            jGroupName merged `shouldBe` ListColumn [GroupName "SamplePop"]
            jLatitude merged  `shouldBe` make "30.0"
            jLongitude merged `shouldBe` make "30.0"
            jAdditionalColumns merged `shouldBe` CsvNamedRecord (HM.fromList [
                    ("AdditionalColumn1", "A"),
                    ("AdditionalColumn2", "C")
                ])
        it "should correctly merge without fields and override" $ do
            cp <- liftIO $ R.newIORef 0
            merged <- testLog $ mergeRow cp jannoTargetRow jannoSourceRow AllJannoColumns True "Poseidon_ID" "Poseidon_ID"
            jSite merged      `shouldBe` Just (JannoSite "Salzburg")
            jGroupName merged `shouldBe` ListColumn [GroupName "SamplePop2"]
            jLatitude merged  `shouldBe` make "30.0"
            jLongitude merged `shouldBe` make "30.0"
            jAdditionalColumns merged `shouldBe` CsvNamedRecord (HM.fromList [
                    ("AdditionalColumn1", "A"),
                    ("AdditionalColumn2", "B")
                ])
        it "should correctly merge with fields selection and no override" $ do
            cp <- liftIO $ R.newIORef 0
            merged <- testLog $ mergeRow cp jannoTargetRow jannoSourceRow (IncludeJannoColumns ["Group_Name", "Latitude"]) False "Poseidon_ID" "Poseidon_ID"
            jSite merged      `shouldBe` Just (JannoSite "Vienna")
            jGroupName merged `shouldBe` ListColumn [GroupName "SamplePop"]
            jLatitude merged  `shouldBe` make "30.0"
            jLongitude merged `shouldBe` Nothing
            jAdditionalColumns merged `shouldBe` CsvNamedRecord (HM.fromList [
                    ("AdditionalColumn2", "C")
                ])
        it "should correctly merge with negative field selection and no override" $ do
            cp <- liftIO $ R.newIORef 0
            merged <- testLog $ mergeRow cp jannoTargetRow jannoSourceRow (ExcludeJannoColumns ["Latitude"]) False "Poseidon_ID" "Poseidon_ID"
            jSite merged      `shouldBe` Just (JannoSite "Vienna")
            jGroupName merged `shouldBe` ListColumn [GroupName "SamplePop"]
            jLatitude merged  `shouldBe` Nothing
            jLongitude merged `shouldBe` make "30.0"
            jAdditionalColumns merged `shouldBe` CsvNamedRecord (HM.fromList [
                    ("AdditionalColumn1", "A"),
                    ("AdditionalColumn2", "C")
                ])
        it "should correctly merge with fields and override" $ do
            cp <- liftIO $ R.newIORef 0
            merged <- testLog $ mergeRow cp jannoTargetRow jannoSourceRow (IncludeJannoColumns ["Group_Name", "Latitude"]) True "Poseidon_ID" "Poseidon_ID"
            jSite merged      `shouldBe` Just (JannoSite "Vienna")
            jGroupName merged `shouldBe` ListColumn [GroupName "SamplePop2"]
            jLatitude merged  `shouldBe` make "30.0"
            jLongitude merged `shouldBe` Nothing
            jAdditionalColumns merged `shouldBe` CsvNamedRecord (HM.fromList [
                    ("AdditionalColumn2", "C")
                ])

testCoalesceMultipleRows :: Spec
testCoalesceMultipleRows = describe "Poseidon.Jannocoalesce.makeNewJannoRows" $ do
    it "should correctly copy with simple matching" $ do
        newJ <- testLog $ makeNewJannoRows jannoSourceRows jannoTargetRows AllJannoColumns False "Poseidon_ID" "Poseidon_ID" Nothing
        jCountry (newJ !! 0) `shouldBe` Just (JannoCountry "Germany")
        jCountry (newJ !! 1) `shouldBe` Just (JannoCountry "Austria")
        jCountry (newJ !! 2) `shouldBe` Nothing
    it "should correctly copy with simple matching and overwrite" $ do
        newJ <- testLog $ makeNewJannoRows jannoSourceRows jannoTargetRows AllJannoColumns True "Poseidon_ID" "Poseidon_ID" Nothing
        jCountry (newJ !! 0) `shouldBe` Just (JannoCountry "Austria")
        jCountry (newJ !! 1) `shouldBe` Just (JannoCountry "Austria")
    it "should throw with duplicate source keys" $ do
        let s = jannoSourceRows ++ [jannoSourceRows !! 1]
        testLog (makeNewJannoRows s jannoTargetRows AllJannoColumns False "Poseidon_ID" "Poseidon_ID" Nothing) `shouldThrow` anyException
    it "should correctly copy with suffix strip" $ do
        newJ <- testLog $ makeNewJannoRows jannoSourceRows jannoTargetRows AllJannoColumns False "Poseidon_ID" "Poseidon_ID" (Just "_AB")
        jCountry (newJ !! 0) `shouldBe` Just (JannoCountry "Germany")
        jCountry (newJ !! 1) `shouldBe` Just (JannoCountry "Austria")
        jCountry (newJ !! 2) `shouldBe` Just (JannoCountry "Austria")
    it "should correctly copy with alternative ID column" $ do
        newJ <- testLog $ makeNewJannoRows jannoSourceRows jannoTargetRows AllJannoColumns False "Poseidon_ID_alt" "Poseidon_ID" Nothing
        jCountry (newJ !! 2) `shouldBe` Just (JannoCountry "Austria")
