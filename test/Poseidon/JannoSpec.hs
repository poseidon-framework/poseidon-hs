module Poseidon.JannoSpec (spec) where

import           Poseidon.Janno            (PoseidonSample (..),
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
    let minimalFullJannoPath    = "test/testDat/testJannoFiles/minimal_full.janno"
    let minimalPartialJannoPath = "test/testDat/testJannoFiles/minimal_partial.janno"
    let normalFullJannoPath     = "test/testDat/testJannoFiles/normal_full.janno"
    let normalPartialJannoPath  = "test/testDat/testJannoFiles/normal_partial.janno"
    let borkedFullJannoPath     = "test/testDat/testJannoFiles/borked_full.janno"
    let borkedPartialJannoPath  = "test/testDat/testJannoFiles/borked_partial.janno"
    it "should read a minimal janno file correctly" $ do
        janno <- readJannoFile minimalFullJannoPath
        janno_partial <- readJannoFile minimalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map posSamIndividualID janno   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map posSamCollectionID janno   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamSourceTissue janno   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamLatitude janno       `shouldBe` [Nothing, Nothing, Nothing]
        map posSamLongitude janno      `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDateC14UncalBP janno `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDateBCADMedian janno `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDateType janno       `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDataType janno       `shouldBe` [Nothing, Nothing, Nothing]
        map posSamGenotypePloidy janno `shouldBe` [Nothing, Nothing, Nothing]
        map posSamGroupName janno      `shouldBe` [["POP1"], ["POP2"], ["POP1"]]
        map posSamGeneticSex janno     `shouldBe` [Male, Female, Male]
        map posSamCoverage1240K janno  `shouldBe` [Nothing, Nothing, Nothing]
        map posSamUDG janno            `shouldBe` [Nothing, Nothing, Nothing]
        map posSamLibraryBuilt janno   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDamage janno         `shouldBe` [Nothing, Nothing, Nothing]
    it "should read a normal janno file correctly" $ do
        janno <- readJannoFile normalFullJannoPath
        janno_partial <- readJannoFile normalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map posSamIndividualID janno   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map posSamCollectionID janno   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamSourceTissue janno   `shouldBe` [Just ["xxx", "yyy"], Just ["xxx"], Just ["xxx"]]
        map posSamCountry janno        `shouldBe` [Just "xxx", Just "xxx", Just "xxx"]
        map posSamLatitude janno       `shouldBe` [Just (Latitude 0), Just (Latitude (-90)), Just (Latitude 90)]
        map posSamLongitude janno      `shouldBe` [Just (Longitude 0), Just (Longitude (-180)), Just (Longitude 180)]
        map posSamDateC14UncalBP janno `shouldBe` [Just [3000, 3100, 2900], Nothing, Nothing]
        map posSamDateBCADMedian janno `shouldBe` [Just (-1000), Just (-5000), Just 2000]
        map posSamDateType janno       `shouldBe` [Just C14, Just Contextual, Just Modern]
        map posSamDataType janno       `shouldBe` [Just [Shotgun, A1240K], Just [A1240K], Just [ReferenceGenome]]
        map posSamGenotypePloidy janno `shouldBe` [Just Diploid, Just Haploid, Just Diploid]
        map posSamGroupName janno      `shouldBe` [["POP1", "POP3"], ["POP2"], ["POP1"]]
        map posSamGeneticSex janno     `shouldBe` [Male, Female, Male]
        map posSamCoverage1240K janno  `shouldBe` [Just 0, Just 0, Just 0]
        map posSamUDG janno            `shouldBe` [Just Minus, Just Half, Just Plus]
        map posSamLibraryBuilt janno   `shouldBe` [Just DS, Just SS, Just Other]
        map posSamDamage janno         `shouldBe` [Just (Percent 0), Just (Percent 100), Just (Percent 50)]
    it "should gracefully fail to read a borked janno file" $ do
        readJannoFile borkedFullJannoPath `shouldThrow` anyException
        readJannoFile borkedPartialJannoPath `shouldThrow` anyException

