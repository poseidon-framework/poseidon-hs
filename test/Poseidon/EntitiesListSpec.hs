{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntitiesListSpec (spec) where

import           Poseidon.EntityTypes
import           Poseidon.Package     (PackageReadOptions (..),
                                       defaultPackageReadOptions,
                                       filterToRelevantPackages,
                                       getJointIndividualInfo,
                                       readPoseidonPackageCollection)
import           Poseidon.Utils       (PoseidonException, testLog)

import           Data.Aeson           (decode, encode)
import           Data.Either          (fromRight, isLeft)
import           Data.Version         (makeVersion)
import           Test.Hspec

spec :: Spec
spec = do
    testReadPoseidonEntitiesString
    testReadEntitiesFromFile
    testFindNonExistentEntities
    testFilterPackages
    testResolveEntityIndices
    testShow
    testJSON


testReadPoseidonEntitiesString :: Spec
testReadPoseidonEntitiesString =
    describe "Poseidon.EntitiesList.readPoseidonEntitiesString" $ do
    it "should parse single entity lists correctly" $ do
        fromRight [] (readEntitiesFromString "<a>") `shouldBe`
            [Include $ Ind "a"]
        fromRight [] (readEntitiesFromString "<c:b:a>") `shouldBe`
            [Include $ SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)]
        fromRight [] (readEntitiesFromString "<c-2.0.2:b:a>") `shouldBe`
            [Include $ SpecificInd "a" "b" (PacNameAndVersion "c" (Just $ makeVersion [2,0,2]))]
        fromRight [] (readEntitiesFromString "b") `shouldBe`
            [Include $ Group "b"]
        fromRight [] (readEntitiesFromString "*c*") `shouldBe`
            [Include $ Pac (PacNameAndVersion "c" Nothing)]
        fromRight [] (readEntitiesFromString "*c-1.2.3*") `shouldBe`
            [Include $ Pac (PacNameAndVersion "c" (Just $ makeVersion [1,2,3]))]
    it "should parse longer entity lists correctly" $ do
        fromRight [] (readEntitiesFromString "<a>,<c:b:a>,b,*c*") `shouldBe`
            map Include [
              Ind "a"
            , SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)
            , Group "b", Pac (PacNameAndVersion "c" Nothing)
            ]
        fromRight [] (readEntitiesFromString "<a1>,b1,<a2>,*c*,b2,<c:b2:a3>") `shouldBe`
            map Include [
              Ind "a1"
            , Group "b1"
            , Ind "a2"
            , Pac (PacNameAndVersion "c" Nothing)
            , Group "b2"
            , SpecificInd "a3" "b2" (PacNameAndVersion "c" Nothing)
            ]
        fromRight [] (readEntitiesFromString "<a1>,*c-4.3.2*,<c-3.3.3:b2:a3>") `shouldBe`
            map Include [
              Ind "a1"
            , Pac (PacNameAndVersion "c" (Just $ makeVersion [4,3,2]))
            , SpecificInd"a3" "b2" (PacNameAndVersion "c" (Just $ makeVersion [3,3,3]))
            ]
    it "should parse unsigned entity lists correctly" $ do
        fromRight [] (readEntitiesFromString "<a>,<c:b:a>,b,*c-1.0.0*") `shouldBe`
            [ Ind "a"
            , SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)
            , Group "b"
            , Pac (PacNameAndVersion "c" (Just $ makeVersion [1,0,0]))
            ]
        fromRight [] (readEntitiesFromString "<a1>,b1,<a2>,*c*,b2,<c-1.0.0:b2:a3>") `shouldBe`
            [ Ind "a1"
            , Group "b1"
            , Ind "a2"
            , Pac (PacNameAndVersion "c" Nothing)
            , Group "b2"
            , SpecificInd "a3" "b2" (PacNameAndVersion "c" (Just $ makeVersion [1,0,0]))
            ]
    it "should ignore spaces after commas" $ do
        fromRight [] (readEntitiesFromString "<a>, <c-1.0.0:b:a>, b, *c*") `shouldBe`
            map Include [
              Ind "a"
            , SpecificInd "a" "b" (PacNameAndVersion "c" (Just $ makeVersion [1,0,0]))
            , Group "b"
            , Pac (PacNameAndVersion "c" Nothing)
            ]
        fromRight [] (readEntitiesFromString "*c*,  b") `shouldBe`
            map Include [
              Pac (PacNameAndVersion "c" Nothing)
            , Group "b"
            ]
    it "should parse exclusion entities correctly" $ do
        fromRight [] (readEntitiesFromString "-<a>") `shouldBe`
            [Exclude $ Ind "a"]
        fromRight [] (readEntitiesFromString "-<a1>, -<c:b:a>, <a2>, -b1,b2,-*c1-1.0.0*, *c2*") `shouldBe`
            [ Exclude $ Ind "a1"
            , Exclude $ SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)
            , Include $ Ind "a2"
            , Exclude $ Group "b1", Include $ Group "b2"
            , Exclude $ Pac (PacNameAndVersion "c1" (Just $ makeVersion [1,0,0]))
            , Include $ Pac (PacNameAndVersion "c2" Nothing)
            ]
    it "should fail with any other setting" $ do
        -- the following type annotations - annoyingly - are required because readEntitiesFromString is polymorphic,
        -- and even though it all returns Left, the compiler complains about ambiguous types.
        (readEntitiesFromString "<a> ,b,*c*"       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString " <a>,b,*c*"       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a >,b,*c*"       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "< a>,b,*c*"       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b,*c* "       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b d,*c*"      :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>>,b,*c*"       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b,*c*c*"      :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "-<a>,b,*c*c*"     :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<a>,b,*c*-"       :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "-a>,b,*c*"        :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<cb:a>"           :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<c:ba>"           :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<c :b:a>"         :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<c: b:a>"         :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<c-1.0.0 :b:a>"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft
        (readEntitiesFromString "<c- 1.0.0:b:a>"   :: Either PoseidonException EntitiesList) `shouldSatisfy` isLeft

testReadEntitiesFromFile :: Spec
testReadEntitiesFromFile =
    describe "Poseidon.EntitiesList.readEntitiesFromFile" $ do
    let g1 = "test/testDat/testEntityFiles/goodEntities1.txt"
        g2 = "test/testDat/testEntityFiles/goodEntities2.txt"
        g3 = "test/testDat/testEntityFiles/goodEntities3.txt"
        g4 = "test/testDat/testEntityFiles/goodEntities4.txt"
        b1 = "test/testDat/testEntityFiles/badEntities1.txt"
    it "should parse good, single-value-per-line files correctly" $ do
        g1res <- readEntitiesFromFile g1
        g1res `shouldBe`
            map Include [
              Ind "a"
            , SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)
            , Group "b"
            , Pac (PacNameAndVersion "c" (Just $ makeVersion [1,0,0]))
            ]
    it "should parse good, multi-value-per-line files correctly" $ do
        g2res <- readEntitiesFromFile g2
        g2res `shouldBe`
            map Include [
              Ind "a1"
            , SpecificInd "a3" "b2" (PacNameAndVersion "c" (Just $ makeVersion [1,0,0]))
            , Ind "a2"
            , Group "b1", Pac (PacNameAndVersion "c1" Nothing)
            , Pac (PacNameAndVersion "c2" Nothing)
            , Group "b2"
            , Group "b3"
            ]
    it "should handle empty lines and #-comments correctly" $ do
        g3res <- readEntitiesFromFile g3
        g3res `shouldBe`
            map Include [
              Ind "a1"
            , Ind "a2"
            , Group "b1"
            , Group "b2"
            , Group "b3"
            ]
    it "should handle exclusion correctly" $ do
        g4res <- readEntitiesFromFile g4
        g4res `shouldBe`
            [ Include $ Ind "a1",
              Exclude $ SpecificInd "a3" "b2" (PacNameAndVersion "c" Nothing)
            , Exclude $ Ind "a2"
            , Exclude $ Group "b1", Include $ Group "b1"
            , Exclude $ Pac (PacNameAndVersion "c2" (Just $ makeVersion [5,5,5]))
            ]
    it "should fail to parse bad files and throw an exception" $ do
        (readEntitiesFromFile b1 :: IO EntitiesList) `shouldThrow` anyException -- wrong space

testPacReadOpts :: PackageReadOptions
testPacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

testBaseDir :: [FilePath]
testBaseDir = ["test/testDat/testPackages/ancient"]

goodEntities :: SignedEntitiesList
goodEntities = [
      Include $ Pac (PacNameAndVersion "Schiffels_2016" (Just $ makeVersion [1,0,1]))
    , Include $ Group "POP1"
    , Include $ Ind "SAMPLE3"
    , Exclude $ Pac (PacNameAndVersion "Schmid_2028" Nothing)
    , Include $ SpecificInd "XXX001" "POP1" (PacNameAndVersion "Schiffels_2016" Nothing)
    , Include $ SpecificInd "XXX012" "POP2" (PacNameAndVersion "Lamnidis_2018" (Just $ makeVersion [1,0,1]))
    ]

badEntities :: EntitiesList
badEntities = [
      Pac (PacNameAndVersion "Schiffels_2015" Nothing)
    , Group "foo"
    , Ind "bar"
    , SpecificInd "XXX002" "POP1" (PacNameAndVersion "Schiffels_2016" (Just $ makeVersion [1,0,2]))
    , SpecificInd "XXX001" "POP2" (PacNameAndVersion "Schiffels_2016" Nothing)
    ]

testFindNonExistentEntities :: Spec
testFindNonExistentEntities =
    describe "Poseidon.EntitiesList.determineNonExistentEntities" $ do
    it "should ignore good entities" $ do
        ps <- testLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let ents = determineNonExistentEntities goodEntities (getJointIndividualInfo ps)
        ents `shouldBe` []
    it "should find bad entities" $ do
        ps <- testLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let ents = determineNonExistentEntities badEntities (getJointIndividualInfo ps)
        ents `shouldMatchList` badEntities

testFilterPackages :: Spec
testFilterPackages =
    describe "Poseidon.EntitiesList.filterToRelevantPackages" $ do
    it "should select all relevant packages" $ do
        ps <- testLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let pacs = filterToRelevantPackages goodEntities ps
        map getPacName pacs `shouldMatchList` ["Schiffels_2016", "Wang_2020", "Lamnidis_2018"]
    it "should drop all irrelevant packages" $ do
        ps <- testLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let pacs = filterToRelevantPackages badEntities ps
        pacs `shouldBe` []

testResolveEntityIndices :: Spec
testResolveEntityIndices =
    describe "Poseidon.EntitiesList.resolveEntityIndices" $ do
    it "should select all relevant individuals" $ do
        ps <- testLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let indInts = resolveEntityIndices goodEntities (getJointIndividualInfo ps)
        indInts `shouldBe` [0, 1, 2, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 25]
    it "should drop all irrelevant individuals" $ do
        ps <- testLog $ readPoseidonPackageCollection testPacReadOpts testBaseDir
        let indInts = resolveEntityIndices badEntities (getJointIndividualInfo ps)
        indInts `shouldBe` []
    it "should correctly extract indices with ordered signed entities" $ do
        let indInfo = [
                  IndividualInfo "Ind1" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" Nothing)
                , IndividualInfo "Ind2" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" Nothing)
                , IndividualInfo "Ind3" ["Pop2", "PopB"] (PacNameAndVersion "Pac1" Nothing)
                , IndividualInfo "Ind4" ["Pop2", "PopB"] (PacNameAndVersion "Pac1" Nothing)
                , IndividualInfo "Ind5" ["Pop3", "PopC"] (PacNameAndVersion "Pac2" Nothing)
                , IndividualInfo "Ind6" ["Pop3", "PopC"] (PacNameAndVersion "Pac2" Nothing)
                , IndividualInfo "Ind7" ["Pop4", "PopC"] (PacNameAndVersion "Pac2" Nothing)
                , IndividualInfo "Ind8" ["Pop4", "PopC"] (PacNameAndVersion "Pac2" Nothing)
                ]
        resolveEntityIndices [
              Include (Pac (PacNameAndVersion "Pac1" Nothing))
            ] indInfo `shouldBe` [0, 1, 2, 3]
        resolveEntityIndices [
              Include (Pac (PacNameAndVersion "Pac1" Nothing))
            , Exclude (Group "Pop2")
            , Include (Ind "Ind3")
            , Include (SpecificInd "Ind8" "Pop4" (PacNameAndVersion "Pac2" Nothing))
            ] indInfo `shouldBe` [0, 1, 2, 7]
    it "should correctly extract indices in case of duplicates across packages" $ do
        let indInfoDuplicates = [
                  IndividualInfo "Ind1" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" Nothing)
                , IndividualInfo "Ind1" ["Pop1", "PopB"] (PacNameAndVersion "Pac2" Nothing)
                , IndividualInfo "Ind1" ["Pop1", "PopB"] (PacNameAndVersion "Pac3" Nothing)
                , IndividualInfo "Ind2" ["Pop2", "PopB"] (PacNameAndVersion "Pac1" Nothing)
                , IndividualInfo "Ind2" ["Pop2", "PopB"] (PacNameAndVersion "Pac2" Nothing)
                , IndividualInfo "Ind2" ["Pop2", "PopB"] (PacNameAndVersion "Pac3" Nothing)
                ]
        -- test simple extraction with specific syntax
        resolveEntityIndices [
               Include (SpecificInd "Ind1" "Pop1" (PacNameAndVersion "Pac2" Nothing))
            ] indInfoDuplicates `shouldBe` [1]
        -- test solving simple duplication for one individual
        resolveEntityIndices [
              Include $ Ind "Ind1"
            , Exclude $ Pac (PacNameAndVersion "Pac2" Nothing)
            , Exclude $ Pac (PacNameAndVersion "Pac3" Nothing)
            ] indInfoDuplicates `shouldBe` [0]
        -- test output in case of unresolved duplicates
        let selectedIndices = resolveEntityIndices [Include (Ind "Ind2")] indInfoDuplicates
        selectedIndices `shouldBe` [3, 4, 5]
        let duplicateReport = reportDuplicateIndividuals . map (indInfoDuplicates !!) $ selectedIndices
        duplicateReport `shouldBe` [
                (IndividualInfo "Ind2" ["Pop2", "PopB"] (PacNameAndVersion "Pac1" Nothing),
                    [SpecificInd "Ind2" "Pop2" (PacNameAndVersion "Pac1" Nothing),
                     SpecificInd "Ind2" "Pop2" (PacNameAndVersion "Pac2" Nothing),
                     SpecificInd "Ind2" "Pop2" (PacNameAndVersion "Pac3" Nothing)])
            ]
        -- test interaction with secondary group name selection and negative selection to solve duplication
        resolveEntityIndices [
              Include $ Group "PopB"
            , Exclude $ Group "Pop2"
            , Exclude $ Pac (PacNameAndVersion "Pac2" Nothing)
            , Exclude $ Pac (PacNameAndVersion "Pac3" Nothing)
            ] indInfoDuplicates `shouldBe` [0]
    it "should correctly extract indices in case of multiple package versions" $ do
        let indInfo = [
                  IndividualInfo "Ind1" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" (Just $ makeVersion [1,0,0]))
                , IndividualInfo "Ind2" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" (Just $ makeVersion [1,0,0]))
                , IndividualInfo "Ind1" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" (Just $ makeVersion [2,0,0]))
                , IndividualInfo "Ind2" ["Pop1", "PopB"] (PacNameAndVersion "Pac1" (Just $ makeVersion [2,0,0]))
                , IndividualInfo "Ind3" ["Pop3", "PopC"] (PacNameAndVersion "Pac2" Nothing)
                , IndividualInfo "Ind4" ["Pop3", "PopC"] (PacNameAndVersion "Pac2" Nothing)
                ]
        let selectedIndices = resolveEntityIndices [Include $ Pac (PacNameAndVersion "Pac1" Nothing)] indInfo
        selectedIndices `shouldBe` [2, 3]
        resolveEntityIndices [
              Include (Pac (PacNameAndVersion "Pac1" (Just $ makeVersion [1,0,0])))
            ] indInfo `shouldBe` [0,1]
        resolveEntityIndices [
              Include (Pac (PacNameAndVersion "Pac1" (Just $ makeVersion [2,0,0])))
            ] indInfo `shouldBe` [2,3]
        resolveEntityIndices [
              Include (SpecificInd "Ind1" "Pop1" (PacNameAndVersion "Pac1" (Just $ makeVersion [2,0,0])))
            , Include (SpecificInd "Ind1" "Pop1" (PacNameAndVersion "Pac1" (Just $ makeVersion [1,0,0])))
            , Exclude (Pac (PacNameAndVersion "Pac1" (Just $ makeVersion [1,0,0])))
            ] indInfo `shouldBe` [2]
        resolveEntityIndices [
              Include (SpecificInd "Ind1" "Pop1" (PacNameAndVersion "Pac1" (Just $ makeVersion [2,0,0])))
            , Include (SpecificInd "Ind1" "Pop1" (PacNameAndVersion "Pac1" (Just $ makeVersion [1,0,0])))
            , Exclude (Pac (PacNameAndVersion "Pac1" Nothing))
            , Include (Ind "Ind4")
            ] indInfo `shouldBe` [5]

testShow :: Spec
testShow =
    describe "Poseidon.EntitiesList.Show" $ do
        it "should turn entities correctly to Strings" $ do
            show (Ind "Ind1") `shouldBe`
                    "<Ind1>"
            show (SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)) `shouldBe`
                    "<c:b:a>"
            show (Group "Group1") `shouldBe`
                    "Group1"
            show (Pac (PacNameAndVersion "Pac1" Nothing)) `shouldBe`
                    "*Pac1*"
            show (Exclude (Ind "Ind1")) `shouldBe`
                    "-<Ind1>"
            show (Exclude (SpecificInd "a" "b" (PacNameAndVersion "c" Nothing))) `shouldBe`
                    "-<c:b:a>"
            show (Exclude (Group "Group1")) `shouldBe`
                    "-Group1"
            show (Exclude (Pac (PacNameAndVersion "Pac1" Nothing))) `shouldBe`
                    "-*Pac1*"

testJSON :: Spec
testJSON =
    describe "Poseidon.EntitiesList.ToJSON" $ do
        it "should encode entities correctly to JSON" $ do
            encode (Ind "Ind1") `shouldBe`
                    "\"<Ind1>\""
            encode (SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)) `shouldBe`
                    "\"<c:b:a>\""
            encode (Group "Group1") `shouldBe`
                    "\"Group1\""
            encode (Pac (PacNameAndVersion "Pac1" Nothing)) `shouldBe`
                    "\"*Pac1*\""
            encode (Exclude (Ind "Ind1")) `shouldBe`
                    "\"-<Ind1>\""
            encode (Exclude (SpecificInd "a" "b" (PacNameAndVersion "c" Nothing))) `shouldBe`
                    "\"-<c:b:a>\""
            encode (Exclude (Group "Group1")) `shouldBe`
                    "\"-Group1\""
            encode (Exclude (Pac (PacNameAndVersion "Pac1" Nothing))) `shouldBe`
                    "\"-*Pac1*\""
        it "should decode entities correctly from JSON" $ do
            decode "\"<Ind1>\""   `shouldBe`
                    Just (Ind "Ind1")
            decode "\"<c:b:a>\""  `shouldBe`
                    Just (SpecificInd "a" "b" (PacNameAndVersion "c" Nothing))
            decode "\"Group1\""   `shouldBe`
                    Just (Group "Group1")
            decode "\"*Pac1*\""   `shouldBe`
                    Just (Pac (PacNameAndVersion "Pac1" Nothing))
            decode "\"-<Ind1>\""  `shouldBe`
                    Just (Exclude (Ind "Ind1"))
            decode "\"-<c:b:a>\"" `shouldBe`
                    Just (Exclude (SpecificInd "a" "b" (PacNameAndVersion "c" Nothing)))
            decode "\"-Group1\""  `shouldBe`
                    Just (Exclude (Group "Group1"))
            decode "\"-*Pac1*\""  `shouldBe`
                    Just (Exclude (Pac (PacNameAndVersion "Pac1" Nothing)))
