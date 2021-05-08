module Poseidon.JannoSpec (spec) where

import           Poseidon.Janno            (JannoRow (..),
                                            JannoSex (..),
                                            Sex (..),
                                            Latitude (..),
                                            Longitude (..),
                                            JannoDateType (..),
                                            JannoDataType (..),
                                            JannoGenotypePloidy (..),
                                            Percent (..),
                                            JannoUDG (..),
                                            JannoLibraryBuilt (..),
                                            readJannoFile)
import           Poseidon.Utils            (PoseidonException (..))

import qualified Data.ByteString.Char8     as B
import           Data.Either               (isRight, rights, isLeft, lefts)
import           Test.Hspec

spec = do
    testPoseidonSampleFromJannoFile

testPoseidonSampleFromJannoFile :: Spec
testPoseidonSampleFromJannoFile = describe "Poseidon.Janno.readJannoFile" $ do
    let minimalFullJannoPath      = "test/testDat/testJannoFiles/minimal_full.janno"
    let minimalPartialJannoPath   = "test/testDat/testJannoFiles/minimal_partial.janno"
    let normalFullJannoPath       = "test/testDat/testJannoFiles/normal_full.janno"
    let normalPartialJannoPath    = "test/testDat/testJannoFiles/normal_partial.janno"
    let borkedFullJannoPath       = "test/testDat/testJannoFiles/borked_full.janno"
    let borkedPartialJannoPath    = "test/testDat/testJannoFiles/borked_partial.janno"
    let borkedWrongNameJannoPath  = "test/testDat/testJannoFiles/borked_wrong_name.janno"
    it "should read minimal janno files correctly" $ do
        janno <- readJannoFile minimalFullJannoPath
        janno_partial <- readJannoFile minimalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map jIndividualID janno   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map jCollectionID janno   `shouldBe` [Nothing, Nothing, Nothing]
        map jSourceTissue janno   `shouldBe` [Nothing, Nothing, Nothing]
        map jLatitude janno       `shouldBe` [Nothing, Nothing, Nothing]
        map jLongitude janno      `shouldBe` [Nothing, Nothing, Nothing]
        map jDateC14UncalBP janno `shouldBe` [Nothing, Nothing, Nothing]
        map jDateBCADMedian janno `shouldBe` [Nothing, Nothing, Nothing]
        map jDateType janno       `shouldBe` [Nothing, Nothing, Nothing]
        map jDataType janno       `shouldBe` [Nothing, Nothing, Nothing]
        map jGenotypePloidy janno `shouldBe` [Nothing, Nothing, Nothing]
        map jGroupName janno      `shouldBe` [["POP1"], ["POP2"], ["POP1"]]
        map jGeneticSex janno     `shouldBe` [JannoSex Male, JannoSex Female, JannoSex Male]
        map jCoverage1240K janno  `shouldBe` [Nothing, Nothing, Nothing]
        map jUDG janno            `shouldBe` [Nothing, Nothing, Nothing]
        map jLibraryBuilt janno   `shouldBe` [Nothing, Nothing, Nothing]
        map jDamage janno         `shouldBe` [Nothing, Nothing, Nothing]
    it "should read normal janno files correctly" $ do
        janno <- readJannoFile normalFullJannoPath
        janno_partial <- readJannoFile normalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map jIndividualID janno   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map jCollectionID janno   `shouldBe` [Nothing, Nothing, Nothing]
        map jSourceTissue janno   `shouldBe` [Just ["xxx", "yyy"], Just ["xxx"], Just ["xxx"]]
        map jCountry janno        `shouldBe` [Just "xxx", Just "xxx", Just "xxx"]
        map jLatitude janno       `shouldBe` [Just (Latitude 0), Just (Latitude (-90)), Just (Latitude 90)]
        map jLongitude janno      `shouldBe` [Just (Longitude 0), Just (Longitude (-180)), Just (Longitude 180)]
        map jDateC14UncalBP janno `shouldBe` [Just [3000, 3100, 2900], Nothing, Nothing]
        map jDateBCADMedian janno `shouldBe` [Just (-1000), Just (-5000), Just 2000]
        map jDateType janno       `shouldBe` [Just C14, Just Contextual, Just Modern]
        map jDataType janno       `shouldBe` [Just [Shotgun, A1240K], Just [A1240K], Just [ReferenceGenome]]
        map jGenotypePloidy janno `shouldBe` [Just Diploid, Just Haploid, Just Diploid]
        map jGroupName janno      `shouldBe` [["POP1", "POP3"], ["POP2"], ["POP1"]]
        map jGeneticSex janno     `shouldBe` [JannoSex Male, JannoSex Female, JannoSex Male]
        map jCoverage1240K janno  `shouldBe` [Just 0, Just 0, Just 0]
        map jUDG janno            `shouldBe` [Just Minus, Just Half, Just Plus]
        map jLibraryBuilt janno   `shouldBe` [Just DS, Just SS, Just Other]
        map jDamage janno         `shouldBe` [Just (Percent 0), Just (Percent 100), Just (Percent 50)]
    it "should fail to read borked janno files" $ do
        readJannoFile borkedFullJannoPath `shouldThrow` anyException
        readJannoFile borkedPartialJannoPath `shouldThrow` anyException
        readJannoFile borkedWrongNameJannoPath `shouldThrow` anyException
