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
                                            loadJannoFile)

import qualified Data.ByteString.Char8     as B
import           Data.Either               (isRight, rights)
import           Test.Hspec

spec = do
    testPoseidonSampleFromJannoFile

testPoseidonSampleFromJannoFile :: Spec
testPoseidonSampleFromJannoFile = describe "Poseidon.Janno.loadJannoFile" $ do
    let minimalJannoPath = "test/testDat/testJannoFiles/minimal.janno"
    let normalJannoPath = "test/testDat/testJannoFiles/normal.janno"
    it "should read a minimal janno file correctly" $ do
        janno <- loadJannoFile minimalJannoPath
        length janno `shouldBe` 3
        all isRight janno `shouldBe` True
        map posSamIndividualID (rights janno)   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map posSamCollectionID (rights janno)   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamSourceTissue (rights janno)   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamLatitude (rights janno)       `shouldBe` [Nothing, Nothing, Nothing]
        map posSamLongitude (rights janno)      `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDateC14UncalBP (rights janno) `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDateBCADMedian (rights janno) `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDateType (rights janno)       `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDataType (rights janno)       `shouldBe` [Nothing, Nothing, Nothing]
        map posSamGenotypePloidy (rights janno) `shouldBe` [Nothing, Nothing, Nothing]
        map posSamGroupName (rights janno)      `shouldBe` [["POP1"], ["POP2"], ["POP1"]]
        map posSamGeneticSex (rights janno)     `shouldBe` [Male, Female, Male]
        map posSamCoverage1240K (rights janno)  `shouldBe` [Nothing, Nothing, Nothing]
        map posSamUDG (rights janno)            `shouldBe` [Nothing, Nothing, Nothing]
        map posSamLibraryBuilt (rights janno)   `shouldBe` [Nothing, Nothing, Nothing]
        map posSamDamage (rights janno)         `shouldBe` [Nothing, Nothing, Nothing]
    it "should read a normal janno file correctly" $ do
        janno <- loadJannoFile normalJannoPath
        length janno `shouldBe` 3
        all isRight janno `shouldBe` True
        map posSamIndividualID (rights janno)   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map posSamCollectionID (rights janno)   `shouldBe` [Just "xxx", Just "xxx", Just "xxx"]
        map posSamSourceTissue (rights janno)   `shouldBe` [Just ["xxx", "yyy"], Just ["xxx"], Just ["xxx"]]
        map posSamLatitude (rights janno)       `shouldBe` [Just (Latitude 0), Just (Latitude (-90)), Just (Latitude 90)]
        map posSamLongitude (rights janno)      `shouldBe` [Just (Longitude 0), Just (Longitude (-180)), Just (Longitude 180)]
        map posSamDateC14UncalBP (rights janno) `shouldBe` [Just [3000, 3100, 2900], Nothing, Nothing]
        map posSamDateBCADMedian (rights janno) `shouldBe` [Just (-1000), Just (-5000), Just 2000]
        map posSamDateType (rights janno)       `shouldBe` [Just C14, Just Contextual, Just Modern]
        map posSamDataType (rights janno)       `shouldBe` [Just [Shotgun, A1240K], Just [A1240K], Just [ReferenceGenome]]
        map posSamGenotypePloidy (rights janno) `shouldBe` [Just Diploid, Just Haploid, Just Diploid]
        map posSamGroupName (rights janno)      `shouldBe` [["POP1", "POP3"], ["POP2"], ["POP1"]]
        map posSamGeneticSex (rights janno)     `shouldBe` [Male, Female, Male]
        map posSamCoverage1240K (rights janno)  `shouldBe` [Just 0, Just 0, Just 0]
        map posSamUDG (rights janno)            `shouldBe` [Just Minus, Just Half, Just Plus]
        map posSamLibraryBuilt (rights janno)   `shouldBe` [Just DS, Just SS, Just Other]
        map posSamDamage (rights janno)         `shouldBe` [Just (Percent 0), Just (Percent 100), Just (Percent 50)]
