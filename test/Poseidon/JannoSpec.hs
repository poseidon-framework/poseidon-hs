{-# LANGUAGE OverloadedStrings #-}

module Poseidon.JannoSpec (spec, checkEnDe) where

import           Poseidon.AccessionIDs
import           Poseidon.ColumnTypesJanno
import           Poseidon.ColumnTypesUtils
import           Poseidon.Janno             (CsvNamedRecord (..), JannoRow (..),
                                             JannoRows (..), readJannoFile)
import           Poseidon.Utils             (testLog)

import           Country                    (decodeAlphaTwo)
import qualified Data.Csv                   as C
import           Data.HashMap.Strict        (fromList)
import           SequenceFormats.Eigenstrat (Sex (..))
import           System.FilePath            ((</>))
import           Test.Hspec                 (Spec, anyException, describe, it,
                                             shouldBe, shouldThrow)

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
        checkEnDe [GeneticSex Female, GeneticSex Male, GeneticSex Unknown]
        checkEnDe [JannoDateBCADStart (-100), JannoDateBCADStart 100]
        checkEnDe (enumFrom minBound :: [JannoDateType]) -- get all constructors for JannoDateType in a list
        checkEnDe (enumFrom minBound :: [JannoCaptureType])
        checkEnDe (enumFrom minBound :: [JannoGenotypePloidy])
        checkEnDe (enumFrom minBound :: [JannoUDG])
        checkEnDe (enumFrom minBound :: [JannoLibraryBuilt])
        checkEnDe [JannoCountryISO <$> decodeAlphaTwo "DE", JannoCountryISO <$> decodeAlphaTwo "FR", JannoCountryISO <$> decodeAlphaTwo "KE"]
        checkEnDe [JannoLatitude (-45), JannoLatitude 45]
        checkEnDe [JannoLongitude (-100), JannoLongitude 100]
        checkEnDe [JannoEndogenous 0, JannoEndogenous 100]
        checkEnDe [JannoDataPreparationPipelineURL "http://www.google.de"]
        checkEnDe (enumFrom minBound :: [JannoRelationDegree])
        checkEnDe [JannoGeneticSourceAccessionID $ INSDCProject "PRJEA0", JannoGeneticSourceAccessionID $ INSDCStudy "ERP000000"]
        checkEnDe [ListColumn (["a", "b", "c"] :: [String])]
        checkEnDe [ListColumn ([1, 2, 3] :: [Int])]
        checkEnDe [ListColumn (enumFrom minBound :: [JannoUDG])] -- to test if ListColumn is really fully general
        -- with Maybe
        checkEnDe ([Nothing, Just $ JannoLatitude (-45), Just $ JannoLatitude 45] :: [Maybe JannoLatitude])

-- infrastructure to check an en- and decoding cycle
checkEnDe :: (Show a, Eq a, C.FromField a, C.ToField a) => [a] -> IO ()
checkEnDe xs = cassavaCycle xs `shouldBe` cassavaResult xs
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
        map jGroupName janno                    `shouldBe` [ListColumn [GroupName "POP1"], ListColumn [GroupName "POP2"], ListColumn [GroupName "POP1"]]
        map jGeneticSex janno                   `shouldBe` [GeneticSex Male, GeneticSex Female, GeneticSex Male]
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
        map jRelationDegree janno               `shouldBe` [Just (ListColumn [First, Second]), Just (ListColumn [First]), Just (ListColumn [SixthToTenth])]
        map jCollectionID janno                 `shouldBe` [Nothing, Nothing, Nothing]
        map jSourceTissue janno                 `shouldBe` [Just (ListColumn [JannoSourceTissue "xxx", JannoSourceTissue "yyy"]), Just (ListColumn [JannoSourceTissue "xxx"]), Just (ListColumn [JannoSourceTissue "xxx"])]
        map jCountry janno                      `shouldBe` [Just (JannoCountry "xxx"), Just (JannoCountry "xxx"), Just (JannoCountry "xxx")]
        map jCountryISO janno                   `shouldBe` [JannoCountryISO <$> decodeAlphaTwo "DE", JannoCountryISO <$> decodeAlphaTwo "FR", JannoCountryISO <$> decodeAlphaTwo "EG"]
        map jLatitude janno                     `shouldBe` [Just (JannoLatitude 0), Just (JannoLatitude (-90)), Just (JannoLatitude 90)]
        map jLongitude janno                    `shouldBe` [Just (JannoLongitude 0), Just (JannoLongitude (-180)), Just (JannoLongitude 180)]
        map jDateC14Labnr janno                 `shouldBe` [Just (ListColumn [JannoDateC14Labnr "A-1", JannoDateC14Labnr "A-2", JannoDateC14Labnr "A-3"]), Nothing, Nothing]
        map jDateC14UncalBP janno               `shouldBe` [Just (ListColumn [JannoDateC14UncalBP 3000, JannoDateC14UncalBP 3100, JannoDateC14UncalBP 2900]), Nothing, Nothing]
        map jDateBCADMedian janno               `shouldBe` [Just (JannoDateBCADMedian (-1000)), Just (JannoDateBCADMedian (-5000)), Just (JannoDateBCADMedian 2000)]
        map jDateType janno                     `shouldBe` [Just C14, Just Contextual, Just Modern]
        map jLibraryNames janno                 `shouldBe` [Just $ ListColumn [JannoLibraryName "Lib1", JannoLibraryName "Lib2"], Just $ ListColumn [JannoLibraryName "Lib3"], Nothing]
        map jCaptureType janno                  `shouldBe` [Just (ListColumn [Shotgun, A1240K]), Just (ListColumn [A1240K]), Just (ListColumn [ReferenceGenome])]
        map jGenotypePloidy janno               `shouldBe` [Just Diploid, Just Haploid, Just Diploid]
        map jGroupName janno                    `shouldBe` [ListColumn [GroupName "POP1", GroupName "POP3"], ListColumn [GroupName "POP2"], ListColumn [GroupName "POP1"]]
        map jGeneticSex janno                   `shouldBe` [GeneticSex Male, GeneticSex Female, GeneticSex Male]
        map jCoverageOnTargets janno            `shouldBe` [Just $ JannoCoverageOnTargets 0, Just $ JannoCoverageOnTargets 0, Just $ JannoCoverageOnTargets 0]
        map jUDG janno                          `shouldBe` [Just Minus, Just Half, Just Plus]
        map jLibraryBuilt janno                 `shouldBe` [Just DS, Just SS, Just MixedSSDS]
        map jDamage janno                       `shouldBe` [Just (JannoDamage 0), Just (JannoDamage 100), Just (JannoDamage 50)]
        map jContamination janno                `shouldBe` [Just (ListColumn [JannoContamination "10"]), Just (ListColumn [JannoContamination "20", JannoContamination "50", JannoContamination "70"]), Nothing]
        map jDataPreparationPipelineURL janno   `shouldBe` [Just (JannoDataPreparationPipelineURL "ftp://test.test"),
                                                            Just (JannoDataPreparationPipelineURL "https://www.google.de"),
                                                            Just (JannoDataPreparationPipelineURL "http://huhu.org/23&test")
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
        testLog (readJannoFile $ borkedDir </> "borked_ISO_country.janno") `shouldThrow` anyException
