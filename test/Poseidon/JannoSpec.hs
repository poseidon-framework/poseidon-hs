module Poseidon.JannoSpec (spec) where

import           Poseidon.Janno            (JannoRow (..),
                                            JannoSex (..),
                                            JannoList (..),
                                            Sex (..),
                                            Latitude (..),
                                            Longitude (..),
                                            JannoDateType (..),
                                            JannoCaptureType (..),
                                            JannoGenotypePloidy (..),
                                            Percent (..),
                                            JannoUDG (..),
                                            JURI (..),
                                            RelationDegree (..),
                                            JannoLibraryBuilt (..),
                                            readJannoFile)

import Test.Hspec
    ( anyException, shouldThrow, shouldBe, it, describe, Spec )

spec :: Spec
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
    it "should read minimal janno files correctly" $ do
        janno <- readJannoFile False minimalFullJannoPath
        janno_partial <- readJannoFile False minimalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map jPoseidonID janno                   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map jCollectionID janno                 `shouldBe` [Nothing, Nothing, Nothing]
        map jSourceTissue janno                 `shouldBe` [Nothing, Nothing, Nothing]
        map jLatitude janno                     `shouldBe` [Nothing, Nothing, Nothing]
        map jLongitude janno                    `shouldBe` [Nothing, Nothing, Nothing]
        map jDateC14UncalBP janno               `shouldBe` [Nothing, Nothing, Nothing]
        map jDateBCADMedian janno               `shouldBe` [Nothing, Nothing, Nothing]
        map jDateType janno                     `shouldBe` [Nothing, Nothing, Nothing]
        map jCaptureType janno                  `shouldBe` [Nothing, Nothing, Nothing]
        map jGenotypePloidy janno               `shouldBe` [Nothing, Nothing, Nothing]
        map jGroupName janno                    `shouldBe` [JannoList ["POP1"], JannoList ["POP2"], JannoList ["POP1"]]
        map jGeneticSex janno                   `shouldBe` [JannoSex Male, JannoSex Female, JannoSex Male]
        map jCoverageOnTargets janno            `shouldBe` [Nothing, Nothing, Nothing]
        map jUDG janno                          `shouldBe` [Nothing, Nothing, Nothing]
        map jLibraryBuilt janno                 `shouldBe` [Nothing, Nothing, Nothing]
        map jDamage janno                       `shouldBe` [Nothing, Nothing, Nothing]
    it "should read normal janno files correctly" $ do
        janno <- readJannoFile False normalFullJannoPath
        janno_partial <- readJannoFile False normalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map jPoseidonID janno                   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map jRelationDegree janno               `shouldBe` [Just (JannoList [First, Second]), Just (JannoList [First]), Just (JannoList [SixthToTenth])]
        map jCollectionID janno                 `shouldBe` [Nothing, Nothing, Nothing]
        map jSourceTissue janno                 `shouldBe` [Just (JannoList ["xxx", "yyy"]), Just (JannoList ["xxx"]), Just (JannoList ["xxx"])]
        map jCountry janno                      `shouldBe` [Just "xxx", Just "xxx", Just "xxx"]
        map jLatitude janno                     `shouldBe` [Just (Latitude 0), Just (Latitude (-90)), Just (Latitude 90)]
        map jLongitude janno                    `shouldBe` [Just (Longitude 0), Just (Longitude (-180)), Just (Longitude 180)]
        map jDateC14Labnr janno                 `shouldBe` [Just (JannoList ["A-1", "A-2", "A-3"]), Nothing, Nothing]
        map jDateC14UncalBP janno               `shouldBe` [Just (JannoList [3000, 3100, 2900]), Nothing, Nothing]
        map jDateBCADMedian janno               `shouldBe` [Just (-1000), Just (-5000), Just 2000]
        map jDateType janno                     `shouldBe` [Just C14, Just Contextual, Just Modern]
        map jCaptureType janno                  `shouldBe` [Just (JannoList [Shotgun, A1240K]), Just (JannoList [A1240K]), Just (JannoList [ReferenceGenome])]
        map jGenotypePloidy janno               `shouldBe` [Just Diploid, Just Haploid, Just Diploid]
        map jGroupName janno                    `shouldBe` [JannoList ["POP1", "POP3"], JannoList ["POP2"], JannoList ["POP1"]]
        map jGeneticSex janno                   `shouldBe` [JannoSex Male, JannoSex Female, JannoSex Male]
        map jCoverageOnTargets janno            `shouldBe` [Just 0, Just 0, Just 0]
        map jUDG janno                          `shouldBe` [Just Minus, Just Half, Just Plus]
        map jLibraryBuilt janno                 `shouldBe` [Just DS, Just SS, Just Other]
        map jDamage janno                       `shouldBe` [Just (Percent 0), Just (Percent 100), Just (Percent 50)]
        map jContamination janno                `shouldBe` [Just (JannoList ["10"]), Just (JannoList ["20", "50", "70"]), Nothing]
        map jDataPreparationPipelineURL janno   `shouldBe` [Just (JURI "ftp://test.test"),
                                                          Just (JURI "https://www.google.de"), 
                                                          Just (JURI "http://huhu.org/23&test")]
    -- the following tests should be more precise and comprehensive; we should consider refactoring 
    -- (maybe when we eventually switch to a different error logging strategy)
    it "should fail to read somehow borked janno files" $ do
        readJannoFile False borkedFullJannoPath `shouldThrow` anyException
        readJannoFile False borkedPartialJannoPath `shouldThrow` anyException
    it "should fail to read borked janno files with specific issues" $ do
        readJannoFile False "test/testDat/testJannoFiles/specificallyBorked/borked_wrong_name.janno" `shouldThrow` anyException
        readJannoFile False "test/testDat/testJannoFiles/specificallyBorked/borked_relations.janno" `shouldThrow` anyException
        readJannoFile False "test/testDat/testJannoFiles/specificallyBorked/borked_contamination.janno" `shouldThrow` anyException
        readJannoFile False "test/testDat/testJannoFiles/specificallyBorked/borked_dating.janno" `shouldThrow` anyException
