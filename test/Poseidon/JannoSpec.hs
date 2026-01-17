{-# LANGUAGE OverloadedStrings #-}

module Poseidon.JannoSpec (spec, checkEnDe) where

import           Poseidon.AccessionIDs
import           Poseidon.ColumnTypesJanno
import           Poseidon.ColumnTypesUtils
import           Poseidon.Janno
import           Poseidon.Utils             (testLog)
import           Poseidon.PoseidonVersion

import           Country                    (decodeAlphaTwo)
import qualified Data.Csv                   as C
import qualified Data.HashMap.Strict        as HM
import qualified Data.Vector                as V
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..), Sex (..))
import           System.FilePath            ((</>))
import           Test.Hspec                 (Spec, anyException, describe, it,
                                             shouldBe, shouldContain,
                                             shouldThrow)

spec :: Spec
spec = do
    testMakeHeaderWithAdditionalColumns
    testEnAndDecoding
    testPoseidonSampleFromJannoFile

testMakeHeaderWithAdditionalColumns :: Spec
testMakeHeaderWithAdditionalColumns = describe "Poseidon.Janno: Column sorting (header preparation)" $ do
    it "should sort columns as expected" $ do
        let jannoRowEmpty = createMinimalSample (EigenstratIndEntry "a" Unknown "test")
            jannoRow = jannoRowEmpty {
                jAdditionalColumns = CsvNamedRecord $ HM.fromList [
                  ("Relation_Note","n/a")
                , ("Date_Note","n/a")
                , ("Source_Material_Note","n/a")
                , ("Contamination_Note","n/a")
                , ("Genetic_Sex_Note","n/a")
                , ("AdditionalColumn2","n/a")
                 ,("AdditionalColumn1","n/a")
                ]
            }
            header = V.toList $ makeHeaderWithAdditionalColumns [jannoRow]
        -- this test is not very clever and will also sometimes need adjustment when
        -- something unrelated changes in the .janno column setup
        header `shouldContain` ["Relation_Type", "Relation_Note"]
        header `shouldContain` ["Date_BC_AD_Stop", "Date_Note"]
        header `shouldContain` ["Chromosomal_Anomalies", "MT_Haplogroup"]
        header `shouldContain` ["Source_Material", "Source_Material_Note"]
        header `shouldContain` ["Contamination_Meas", "Contamination_Note"]
        header `shouldContain` ["Genetic_Sex", "Genetic_Sex_Note"]
        header `shouldContain` ["Keywords", "AdditionalColumn1", "AdditionalColumn2"]

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
        checkEnDe (enumFrom minBound :: [JannoSourceMaterial])
        checkEnDe (enumFrom minBound :: [JannoRelationDegree])
        checkEnDe [JannoCountryISO <$> decodeAlphaTwo "DE", JannoCountryISO <$> decodeAlphaTwo "FR", JannoCountryISO <$> decodeAlphaTwo "KE"]
        checkEnDe [JannoLatitude (-45), JannoLatitude 45]
        checkEnDe [JannoLongitude (-100), JannoLongitude 100]
        checkEnDe [JannoEndogenous 0, JannoEndogenous 100]
        checkEnDe [JannoDataPreparationPipelineURL "http://www.google.de"]
        checkEnDe [JannoGeneticSourceAccessionID $ INSDCProject "PRJEA0", JannoGeneticSourceAccessionID $ INSDCStudy "ERP000000"]
        checkEnDe [ListColumn (["a", "b", "c"] :: [String])]
        checkEnDe [ListColumn ([1, 2, 3] :: [Int])]
        checkEnDe [ListColumn (enumFrom minBound :: [JannoUDG])] -- to test if ListColumn is really fully general
        -- with Maybe
        checkEnDe ([Nothing, Just $ JannoLatitude (-45), Just $ JannoLatitude 45] :: [Maybe JannoLatitude])

-- infrastructure to check an en- and decoding cycle
checkEnDe :: (Show a, Eq a, FromFieldVersioned a, C.ToField a) => [a] -> IO ()
checkEnDe xs = cassavaCycle xs `shouldBe` cassavaResult xs
    where
        cassavaCycle :: (FromFieldVersioned a, C.ToField a) => [a] -> [Either String a]
        cassavaCycle = map (C.runParser . (parseFieldVersioned latestPoseidonVersion) . C.toField)
        cassavaResult = map Right


testPoseidonSampleFromJannoFile :: Spec
testPoseidonSampleFromJannoFile = describe "Poseidon.Janno.readJannoFile" $ do
    let minimalJannoPath      = "test/testDat/testJannoFiles/minimal.janno"
    let normalJannoPath       = "test/testDat/testJannoFiles/normal.janno"
    let borkedJannoPath       = "test/testDat/testJannoFiles/borked.janno"
    it "should read minimal janno files correctly" $ do
        (JannoRows janno) <- testLog $ readJannoFile latestPoseidonVersion [] minimalJannoPath
        length janno `shouldBe` 3
        map jPoseidonID janno                   `shouldBe` ["XXX011", "XXX012", "XXX013"]
        map jGroupName janno                    `shouldBe` [ ListColumn [GroupName "POP1"]
                                                           , ListColumn [GroupName "POP2"]
                                                           , ListColumn [GroupName "POP1"]
                                                           ]
        map jGeneticSex janno                   `shouldBe` [GeneticSex Male, GeneticSex Female, GeneticSex Male]
    it "should read normal janno files correctly" $ do
        (JannoRows janno) <- testLog $ readJannoFile latestPoseidonVersion [] normalJannoPath
        length janno `shouldBe` 3
        map jPoseidonID janno                   `shouldBe` [ "XXX011", "XXX012", "XXX013" ]
        map jRelationDegree janno               `shouldBe` [ Just (ListColumn [First, Second])
                                                           , Just (ListColumn [First])
                                                           , Just (ListColumn [SixthToTenth])
                                                           ]
        map jCollectionID janno                 `shouldBe` [ Nothing, Nothing, Nothing ]
        map jCulturalEra janno                  `shouldBe` [ Just (ListColumn [
                                                               JannoCulturalEra "Danish Bronze Age"
                                                             , JannoCulturalEra "Pre-Pottery Neolithic A"])
                                                           , Nothing
                                                           , Nothing
                                                           ]
        map jCulturalEraURL janno               `shouldBe` [ Just (ListColumn [
                                                               JannoCulturalEraURL
                                                                 "https://chronontology.dainst.org/period/Gx4uxaeTCbbg"
                                                             , JannoCulturalEraURL
                                                                 "https://n2t.net/ark:/99152/p0zj6g8ks9s"])
                                                           , Nothing
                                                           , Nothing
                                                           ]
        map jArchaeologicalCulture janno        `shouldBe` [ Nothing
                                                           , Just (ListColumn [
                                                               JannoArchaeologicalCulture
                                                                 "Hallstatt culture (Hungary)"])
                                                           , Nothing
                                                           ]
        map jArchaeologicalCultureURL janno     `shouldBe` [ Nothing
                                                           , Just (ListColumn [
                                                               JannoArchaeologicalCultureURL
                                                                 "https://n2t.net/ark:/99152/p0nxc78fxgt"])
                                                           , Nothing
                                                           ]
        map jSourceMaterial janno               `shouldBe` [ Just (ListColumn [MaterialPetrous, MaterialOther])
                                                           , Just (ListColumn [MaterialSoft])
                                                           , Just (ListColumn [MaterialHair])
                                                           ]
        map jCountry janno                      `shouldBe` [ Just (JannoCountry "xxx")
                                                           , Just (JannoCountry "xxx")
                                                           , Just (JannoCountry "xxx")
                                                           ]
        map jCountryISO janno                   `shouldBe` [ JannoCountryISO <$> decodeAlphaTwo "DE"
                                                           , JannoCountryISO <$> decodeAlphaTwo "FR"
                                                           , JannoCountryISO <$> decodeAlphaTwo "EG"
                                                           ]
        map jLatitude janno                     `shouldBe` [ Just (JannoLatitude 0)
                                                           , Just (JannoLatitude (-90))
                                                           , Just (JannoLatitude 90)
                                                           ]
        map jLongitude janno                    `shouldBe` [ Just (JannoLongitude 0)
                                                           , Just (JannoLongitude (-180))
                                                           , Just (JannoLongitude 180)
                                                           ]
        map jDateC14Labnr janno                 `shouldBe` [ Just (ListColumn [
                                                               JannoDateC14Labnr "A-1"
                                                             , JannoDateC14Labnr "A-2"
                                                             , JannoDateC14Labnr "A-3"])
                                                           , Nothing
                                                           , Nothing
                                                           ]
        map jDateC14UncalBP janno               `shouldBe` [ Just (ListColumn [
                                                               JannoDateC14UncalBP 3000
                                                             , JannoDateC14UncalBP 3100
                                                             , JannoDateC14UncalBP 2900])
                                                           , Nothing
                                                           , Nothing]
        map jDateBCADMedian janno               `shouldBe` [ Just (JannoDateBCADMedian (-1000))
                                                           , Just (JannoDateBCADMedian (-5000))
                                                           , Just (JannoDateBCADMedian 2000)]
        map jDateType janno                     `shouldBe` [ Just C14
                                                           , Just Contextual
                                                           , Just Modern
                                                           ]
        map jLibraryNames janno                 `shouldBe` [ Just $ ListColumn [
                                                               JannoLibraryName "Lib1"
                                                             , JannoLibraryName "Lib2"]
                                                           , Just $ ListColumn [JannoLibraryName "Lib3"]
                                                           , Nothing
                                                           ]
        map jCaptureType janno                  `shouldBe` [ Just (ListColumn [Shotgun, A1240K])
                                                           , Just (ListColumn [A1240K])
                                                           , Just (ListColumn [LegacyReferenceGenome])
                                                           ]
        map jGenotypePloidy janno               `shouldBe` [ Just Diploid
                                                           , Just Haploid
                                                           , Just Diploid
                                                           ]
        map jGroupName janno                    `shouldBe` [ ListColumn [GroupName "POP1", GroupName "POP3"]
                                                           , ListColumn [GroupName "POP2"]
                                                           , ListColumn [GroupName "POP1"]
                                                           ]
        map jGeneticSex janno                   `shouldBe` [ GeneticSex Male
                                                           , GeneticSex Female
                                                           , GeneticSex Male
                                                           ]
        map jCoverageOnTargets janno            `shouldBe` [ Just $ JannoCoverageOnTargets 0
                                                           , Just $ JannoCoverageOnTargets 0
                                                           , Just $ JannoCoverageOnTargets 0
                                                           ]
        map jUDG janno                          `shouldBe` [ Just Minus
                                                           , Just Half
                                                           , Just Plus
                                                           ]
        map jLibraryBuilt janno                 `shouldBe` [ Just DS
                                                           , Just SS
                                                           , Just MixedSSDS
                                                           ]
        map jDamage janno                       `shouldBe` [ Just (ListColumn [JannoDamage 0])
                                                           , Just (ListColumn [JannoDamage 1, JannoDamage 0.1])
                                                           , Just (ListColumn [JannoDamage 0.5])
                                                           ]
        map jContamination janno                `shouldBe` [ Just (ListColumn [JannoContamination "10"])
                                                           , Just (ListColumn [JannoContamination "20"
                                                             , JannoContamination "50"
                                                             , JannoContamination "70"])
                                                           , Nothing
                                                           ]
        map jDataPreparationPipelineURL janno   `shouldBe` [ Just (JannoDataPreparationPipelineURL "ftp://test.test")
                                                           , Just (JannoDataPreparationPipelineURL "https://www.google.de")
                                                           , Just (JannoDataPreparationPipelineURL "http://huhu.org/23&test")
                                                           ]
        map jAdditionalColumns janno            `shouldBe` [ CsvNamedRecord (HM.fromList
                                                              [("AdditionalColumn2","test2")
                                                              ,("AdditionalColumn1","test1")
                                                              ,("Contamination_Note","")
                                                              ,("Date_Note","x x x")
                                                              ,("Relation_Note","yyy")
                                                              ,("Source_Tissue","xxx;yyy")])
                                                           , CsvNamedRecord (HM.fromList
                                                              [("AdditionalColumn2","test4")
                                                              ,("AdditionalColumn1","test3")
                                                              ,("Contamination_Note","xxx")
                                                              ,("Date_Note","yyy")
                                                              ,("Relation_Note","n/a")
                                                              ,("Source_Tissue","xxx")])
                                                           , CsvNamedRecord (HM.fromList
                                                              [("AdditionalColumn2","test6")
                                                              ,("AdditionalColumn1","test5")
                                                              ,("Contamination_Note","n/a")
                                                              ,("Date_Note","n/a")
                                                              ,("Relation_Note","xxx")
                                                              ,("Source_Tissue","xxx")])
                                                           ]

    -- the following tests should be more precise and comprehensive; we should consider refactoring
    -- (maybe when we eventually switch to a different error logging strategy)
    it "should fail to read janno files with missing mandatory columns" $ do
        testLog (readJannoFile latestPoseidonVersion ["Bohrmaschine"] normalJannoPath) `shouldThrow` anyException
    it "should fail to read somehow borked janno files" $ do
        testLog (readJannoFile latestPoseidonVersion [] borkedJannoPath) `shouldThrow` anyException
    it "should fail to read borked janno files with specific issues" $ do
        let borkedDir = "test/testDat/testJannoFiles/specificallyBorked"
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_wrong_name.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_relations.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_contamination.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_dating.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_ISO_country.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_cultural_era.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_arch_cultural_url.janno") `shouldThrow` anyException
        testLog (readJannoFile latestPoseidonVersion [] $ borkedDir </> "borked_alternative_ids_context.janno") `shouldThrow` anyException
