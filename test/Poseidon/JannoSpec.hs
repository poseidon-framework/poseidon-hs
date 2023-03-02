{-# LANGUAGE OverloadedStrings #-}

module Poseidon.JannoSpec (spec, checkEnDe) where

import           Poseidon.Janno      (AccessionID (..), BCADAge (..),
                                      CsvNamedRecord (..), JURI (..),
                                      JannoCaptureType (..), JannoDateType (..),
                                      JannoGenotypePloidy (..),
                                      JannoLibraryBuilt (..), JannoList (..),
                                      JannoRow (..), JannoRows (..),
                                      JannoSex (..), JannoUDG (..),
                                      Latitude (..), Longitude (..),
                                      Percent (..), RelationDegree (..),
                                      Sex (..), makeJannoCountryUnsafe,
                                      readJannoFile)
import           Poseidon.Utils      (testLog)

import           Control.Applicative (liftA2)
import qualified Data.Aeson          as A
import qualified Data.Csv            as C
import           Data.HashMap.Strict (fromList)
import           System.FilePath     ((</>))
import           Test.Hspec          (Expectation, Spec, anyException, describe,
                                      it, shouldBe, shouldThrow)

spec :: Spec
spec = do
    testEnAndDecoding
    testPoseidonSampleFromJannoFile

testEnAndDecoding :: Spec
testEnAndDecoding = describe "Poseidon.Janno: JSON and CSV en- and decoding" $ do
    it "should pass smoothly through all relevant en- and decoding cycles" $ do
        -- generic instances
        checkEnDe (["a", "b", "c"] :: [String])
        checkEnDe ([1, 2, 3] :: [Int])
        -- self defined instances
        checkEnDe [JannoSex Female, JannoSex Male, JannoSex Unknown]
        checkEnDe [BCADAge (-100), BCADAge 100]
        checkEnDe (enumFrom minBound :: [JannoDateType]) -- get all constructors for JannoDateType in a list
        checkEnDe (enumFrom minBound :: [JannoCaptureType])
        checkEnDe (enumFrom minBound :: [JannoGenotypePloidy])
        checkEnDe (enumFrom minBound :: [JannoUDG])
        checkEnDe (enumFrom minBound :: [JannoLibraryBuilt])
        checkEnDe [makeJannoCountryUnsafe "DE", makeJannoCountryUnsafe "FR", makeJannoCountryUnsafe "KE"]
        checkEnDe [Latitude (-45), Latitude 45]
        checkEnDe [Longitude (-100), Longitude 100]
        checkEnDe [Percent 0, Percent 100]
        checkEnDe [JURI "http://www.google.de"]
        checkEnDe (enumFrom minBound :: [RelationDegree])
        checkEnDe [INSDCProject "PRJEA0", INSDCStudy "ERP000000"]
        checkEnDe [JannoList (["a", "b", "c"] :: [String])]
        checkEnDe [JannoList ([1, 2, 3] :: [Int])]
        checkEnDe [JannoList (enumFrom minBound :: [JannoUDG])] -- to test if JannoList is really fully general
        -- with Maybe
        checkEnDe ([Nothing, Just $ Latitude (-45), Just $ Latitude 45] :: [Maybe Latitude])

-- infrastructure to check an en- and decoding cycle
checkEnDe :: (Show a, Eq a, A.FromJSON a, A.ToJSON a, C.FromField a, C.ToField a) => [a] -> IO ()
checkEnDe = liftA2 (>>) checkAeson checkCassava
checkAeson :: (Show a, Eq a, A.FromJSON a, A.ToJSON a) => [a] -> Expectation
checkAeson xs = aesonCycle xs `shouldBe` aesonResult xs
    where
        aesonCycle :: (A.FromJSON a, A.ToJSON a) => [a] -> [Maybe a]
        aesonCycle = map (A.decode . A.encode)
        aesonResult = map Just
checkCassava :: (Show a, Eq a, C.FromField a, C.ToField a) => [a] -> Expectation
checkCassava xs = cassavaCycle xs `shouldBe` cassavaResult xs
    where
        cassavaCycle :: (C.FromField a, C.ToField a) => [a] -> [Either String a]
        cassavaCycle = map (C.runParser . C.parseField . C.toField)
        cassavaResult = map Right


testPoseidonSampleFromJannoFile :: Spec
testPoseidonSampleFromJannoFile = describe "Poseidon.Janno.readJannoFile" $ do
    let minimalFullJannoPath      = "test/testDat/testJannoFiles/minimal_full.janno"
    let minimalPartialJannoPath   = "test/testDat/testJannoFiles/minimal_partial.janno"
    let normalFullJannoPath       = "test/testDat/testJannoFiles/normal_full.janno"
    let normalPartialJannoPath    = "test/testDat/testJannoFiles/normal_partial.janno"
    let borkedFullJannoPath       = "test/testDat/testJannoFiles/borked_full.janno"
    let borkedPartialJannoPath    = "test/testDat/testJannoFiles/borked_partial.janno"
    it "should read minimal janno files correctly" $ do
        (JannoRows janno) <- testLog $ readJannoFile minimalFullJannoPath
        (JannoRows janno_partial) <- testLog $ readJannoFile minimalPartialJannoPath
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
        (JannoRows janno) <- testLog $ readJannoFile normalFullJannoPath
        (JannoRows janno_partial) <- testLog $ readJannoFile normalPartialJannoPath
        janno `shouldBe` janno_partial
        length janno `shouldBe` 3
        map jPoseidonID janno                   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map jRelationDegree janno               `shouldBe` [Just (JannoList [First, Second]), Just (JannoList [First]), Just (JannoList [SixthToTenth])]
        map jCollectionID janno                 `shouldBe` [Nothing, Nothing, Nothing]
        map jSourceTissue janno                 `shouldBe` [Just (JannoList ["xxx", "yyy"]), Just (JannoList ["xxx"]), Just (JannoList ["xxx"])]
        map jCountry janno                      `shouldBe` [Just "xxx", Just "xxx", Just "xxx"]
        map jCountryISO janno                   `shouldBe` [Just $ makeJannoCountryUnsafe "DE", Just $ makeJannoCountryUnsafe "FR", Just $ makeJannoCountryUnsafe "EG"]
        map jLatitude janno                     `shouldBe` [Just (Latitude 0), Just (Latitude (-90)), Just (Latitude 90)]
        map jLongitude janno                    `shouldBe` [Just (Longitude 0), Just (Longitude (-180)), Just (Longitude 180)]
        map jDateC14Labnr janno                 `shouldBe` [Just (JannoList ["A-1", "A-2", "A-3"]), Nothing, Nothing]
        map jDateC14UncalBP janno               `shouldBe` [Just (JannoList [3000, 3100, 2900]), Nothing, Nothing]
        map jDateBCADMedian janno               `shouldBe` [Just (BCADAge (-1000)), Just (BCADAge (-5000)), Just (BCADAge 2000)]
        map jDateType janno                     `shouldBe` [Just C14, Just Contextual, Just Modern]
        map jLibraries janno                    `shouldBe` [Just $ JannoList ["Lib1", "Lib2"], Just $ JannoList ["Lib3"], Nothing]
        map jCaptureType janno                  `shouldBe` [Just (JannoList [Shotgun, A1240K]), Just (JannoList [A1240K]), Just (JannoList [ReferenceGenome])]
        map jGenotypePloidy janno               `shouldBe` [Just Diploid, Just Haploid, Just Diploid]
        map jGroupName janno                    `shouldBe` [JannoList ["POP1", "POP3"], JannoList ["POP2"], JannoList ["POP1"]]
        map jGeneticSex janno                   `shouldBe` [JannoSex Male, JannoSex Female, JannoSex Male]
        map jCoverageOnTargets janno            `shouldBe` [Just 0, Just 0, Just 0]
        map jUDG janno                          `shouldBe` [Just $ JannoList [Minus, Minus], Just $ JannoList [Half], Just $ JannoList [Plus]]
        map jLibraryBuilt janno                 `shouldBe` [Just $ JannoList [DS, SS], Just $ JannoList [SS], Just $ JannoList [Other]]
        map jDamage janno                       `shouldBe` [Just (Percent 0), Just (Percent 100), Just (Percent 50)]
        map jContamination janno                `shouldBe` [Just (JannoList ["10"]), Just (JannoList ["20", "50", "70"]), Nothing]
        map jDataPreparationPipelineURL janno   `shouldBe` [Just (JURI "ftp://test.test"),
                                                            Just (JURI "https://www.google.de"),
                                                            Just (JURI "http://huhu.org/23&test")
                                                           ]
        map jAdditionalColumns janno            `shouldBe` [ CsvNamedRecord (fromList [("AdditionalColumn2","test2"),("AdditionalColumn1","test1")])
                                                           , CsvNamedRecord (fromList [("AdditionalColumn2","test4"),("AdditionalColumn1","test3")])
                                                           , CsvNamedRecord (fromList [("AdditionalColumn2","test6"),("AdditionalColumn1","test5")])
                                                           ]

    -- the following tests should be more precise and comprehensive; we should consider refactoring
    -- (maybe when we eventually switch to a different error logging strategy)
    it "should fail to read somehow borked janno files" $ do
        testLog (readJannoFile borkedFullJannoPath) `shouldThrow` anyException
        testLog (readJannoFile borkedPartialJannoPath) `shouldThrow` anyException
    it "should fail to read borked janno files with specific issues" $ do
        let borkedDir = "test/testDat/testJannoFiles/specificallyBorked"
        testLog (readJannoFile $ borkedDir </> "borked_wrong_name.janno") `shouldThrow` anyException
        testLog (readJannoFile $ borkedDir </> "borked_relations.janno") `shouldThrow` anyException
        testLog (readJannoFile $ borkedDir </> "borked_contamination.janno") `shouldThrow` anyException
        testLog (readJannoFile $ borkedDir </> "borked_dating.janno") `shouldThrow` anyException
        testLog (readJannoFile $ borkedDir </> "borked_non_existent_ISO_country.janno") `shouldThrow` anyException
