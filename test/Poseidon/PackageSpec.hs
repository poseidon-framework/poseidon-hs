{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Poseidon.PackageSpec (spec) where

import           Poseidon.Contributor       (ContributorSpec (..), ORCID (..))
import           Poseidon.EntityTypes       (HasNameAndVersion (..))
import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..),
                                             SNPSetSpec (..))
import           Poseidon.Package           (PackageReadOptions (..),
                                             PoseidonPackage (..),
                                             PoseidonYamlStruct (..),
                                             defaultPackageReadOptions,
                                             getJointGenotypeData,
                                             readPoseidonPackage,
                                             readPoseidonPackageCollection,
                                             renderMismatch, zipWithPadding)
import           Poseidon.Utils             (LogMode (..),
                                             PoseidonException (..),
                                             TestMode (..), getChecksum, noLog,
                                             testLog, usePoseidonLogger)

import qualified Data.ByteString.Char8      as B
import           Data.Either                (fromLeft, fromRight)
import           Data.List                  (sort)
import           Data.Time                  (fromGregorian)
import qualified Data.Vector                as V
import           Data.Version               (makeVersion)
import           Data.Yaml                  (ParseException (AesonException),
                                             decodeEither')
import           Pipes.OrderedZip           (WrongInputOrderException (..))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (EigenstratSnpEntry (..),
                                             GenoEntry (..))
import           SequenceFormats.Plink      (PlinkPopNameMode (..))
import           SequenceFormats.Utils      (Chrom (..))
import           Test.Hspec
import           Text.RawString.QQ

spec :: Spec
spec = do
    testPoseidonFromYAML
    testreadPoseidonPackageCollection
    testGetChecksum
    testRenderMismatch
    testZipWithPadding
    testGetJoinGenotypeData
    testThrowOnRead

testPacReadOpts :: PackageReadOptions
testPacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

yamlPackage :: B.ByteString
yamlPackage = [r|
poseidonVersion: 2.0.1
title: Schiffels_2016
description: Genetic data published in Schiffels et al. 2016
contributor:
  - name: Stephan Schiffels
    email: schiffels@institute.org
    orcid: 0000-0002-1017-9150
packageVersion: 1.0.0
lastModified: 2020-02-28
bibFile: sources.bib
genotypeData:
  format: PLINK
  genoFile: Schiffels_2016.bed
  snpFile: Schiffels_2016.bim
  indFile: Schiffels_2016.fam
  snpSet: 1240K
jannoFile: Schiffels_2016.janno
|]

replace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replace from to s =
    let (h, t) = B.breakSubstring from s
    in  B.concat [h, to, B.drop (B.length from) t]

truePackageRelPaths :: PoseidonYamlStruct
truePackageRelPaths = PoseidonYamlStruct {
    _posYamlPoseidonVersion = makeVersion [2, 0, 1],
    _posYamlTitle           = "Schiffels_2016",
    _posYamlDescription     = Just "Genetic data published in Schiffels et al. 2016",
    _posYamlContributor     = [
        ContributorSpec
            "Stephan Schiffels"
            "schiffels@institute.org"
            (Just $ ORCID {_orcidNums = "000000021017915", _orcidChecksum = '0'})
        ],
    _posYamlPackageVersion  = Just $ makeVersion [1, 0, 0],
    _posYamlLastModified    = Just $ fromGregorian 2020 2 28,
    _posYamlGenotypeData    = GenotypeDataSpec {
        format   = GenotypeFormatPlink,
        genoFile = "Schiffels_2016.bed",
        genoFileChkSum = Nothing,
        snpFile  = "Schiffels_2016.bim",
        snpFileChkSum = Nothing,
        indFile  = "Schiffels_2016.fam",
        indFileChkSum = Nothing,
        snpSet = Just SNPSet1240K
    },
    _posYamlJannoFile       = Just "Schiffels_2016.janno",
    _posYamlJannoFileChkSum = Nothing,
    _posYamlSeqSourceFile       = Nothing,
    _posYamlSeqSourceFileChkSum = Nothing,
    _posYamlBibFile         = Just "sources.bib",
    _posYamlBibFileChkSum   = Nothing,
    _posYamlReadmeFile      = Nothing,
    _posYamlChangelogFile   = Nothing
}

testPoseidonFromYAML :: Spec
testPoseidonFromYAML = describe "PoseidonPackage.fromYAML" $ do
    let p = fromRight dummyPackageYamlStruct $ decodeTest yamlPackage
    it "should parse correct YAML data" $
        p `shouldBe` truePackageRelPaths
    it "should give error with incorrect date string" $ do
        let yamlPackage2 = replace "2020-02-28" "2019-07-10sss" yamlPackage
            err = fromLeft dummyParseException $ decodeTest yamlPackage2
        show err `shouldBe` "AesonException \"Error in $.lastModified: could not parse date: endOfInput\""
    it "should give error with incorrect poseidonVersion string" $ do
        let yamlPackage2 = replace "2.0.1" "1.0.0sss" yamlPackage
            err = fromLeft dummyParseException $ decodeTest yamlPackage2
        show err `shouldBe` "AesonException \"Error in $.poseidonVersion: parsing Version failed\""
    it "should give error with incorrect packageVersion string" $ do
        let yamlPackage2 = replace "1.0.0" "a.b.c" yamlPackage
            err = fromLeft dummyParseException $ decodeTest yamlPackage2
        show err `shouldBe` "AesonException \"Error in $.packageVersion: parsing Version failed\""
    it "should give Nothing for missing bibFile" $ do
        let yamlPackage2 = replace "bibFile: sources.bib\n" "" yamlPackage
            p_ = fromRight dummyPackageYamlStruct $ decodeTest yamlPackage2
        p_ `shouldBe` truePackageRelPaths {_posYamlBibFile = Nothing}
    it "should fail with title missing" $ do
        let yamlPackage2 = replace "title: Schiffels_2016\n" "" yamlPackage
            err = fromLeft dummyParseException $ decodeTest yamlPackage2
        show err `shouldBe` "AesonException \"Error in $: key \\\"title\\\" not found\""
    it "should fail with poseidonVersion missing" $ do
        let yamlPackage2 = replace "poseidonVersion: 2.0.1\n" "" yamlPackage
            err = fromLeft dummyParseException $ decodeTest yamlPackage2
        show err `shouldBe` "AesonException \"Error in $: key \\\"poseidonVersion\\\" not found\""
    it "should fail with lastModified missing" $ do
        let yamlPackage2 = replace "lastModified: 2020-02-28\n" "" yamlPackage
            p_ = fromRight dummyPackageYamlStruct $ decodeTest yamlPackage2
        p_ `shouldBe` truePackageRelPaths {_posYamlLastModified = Nothing}
    it "should parse missing snpSet field as Nothing" $ do
        let yamlPackage2 = replace "  snpSet: 1240K\n" "" yamlPackage
            p_ = fromRight dummyPackageYamlStruct $ decodeTest yamlPackage2
            gd = _posYamlGenotypeData p_
            gdTrue = _posYamlGenotypeData truePackageRelPaths
        gd `shouldBe` gdTrue {snpSet = Nothing}
    it "should parse missing contributor field as empty list" $ do
        let yamlPackage2 = replace
                "contributor:\n  - name: Stephan Schiffels\n    email: schiffels@institute.org\n    orcid: 0000-0002-1017-9150" "" yamlPackage
            p_ = fromRight dummyPackageYamlStruct $ decodeTest yamlPackage2
            contri = _posYamlContributor p_
        contri `shouldBe` []
    it "should fail for a wrong ORCID" $ do
        let yamlPackage2 = replace "0000-0002-1017-9150" "0000-0002-1017-9151" yamlPackage
            err = fromLeft dummyParseException $ decodeTest yamlPackage2
        show err `shouldContain` "ORCID is not valid"
    where
        decodeTest :: B.ByteString -> Either ParseException PoseidonYamlStruct
        decodeTest bs = decodeEither' bs
        dummyPackageYamlStruct :: PoseidonYamlStruct
        dummyPackageYamlStruct = PoseidonYamlStruct {
            _posYamlPoseidonVersion = makeVersion [0, 0, 0],
            _posYamlTitle           = "dummyPackage",
            _posYamlDescription     = Nothing,
            _posYamlContributor     = [],
            _posYamlPackageVersion  = Nothing,
            _posYamlLastModified    = Nothing,
            _posYamlGenotypeData    = GenotypeDataSpec {
                format   = GenotypeFormatPlink,
                genoFile = "test.bed",
                genoFileChkSum = Nothing,
                snpFile  = "test.bim",
                snpFileChkSum = Nothing,
                indFile  = "test.fam",
                indFileChkSum = Nothing,
                snpSet = Nothing
            },
            _posYamlJannoFile       = Nothing,
            _posYamlJannoFileChkSum = Nothing,
            _posYamlSeqSourceFile       = Nothing,
            _posYamlSeqSourceFileChkSum = Nothing,
            _posYamlBibFile         = Nothing,
            _posYamlBibFileChkSum   = Nothing,
            _posYamlReadmeFile      = Nothing,
            _posYamlChangelogFile   = Nothing
        }
        dummyParseException :: ParseException
        dummyParseException = AesonException "dummyException"

testreadPoseidonPackageCollection :: Spec
testreadPoseidonPackageCollection = describe "PoseidonPackage.findPoseidonPackages" $ do
    let dir = "test/testDat/testPackages/ancient"
    it "should discover packages correctly" $ do
        pac <- testLog $ readPoseidonPackageCollection testPacReadOpts [dir]
        sort (map getPacName pac) `shouldBe` ["Lamnidis_2018", "Lamnidis_2018", "Schiffels_2016", "Schmid_2028", "Wang_2020"]
        sort (map posPacLastModified pac) `shouldBe` [Just (fromGregorian 2019 01 15),
                                                      Just (fromGregorian 2020 2 20),
                                                      Just (fromGregorian 2020 5 20),
                                                      Just (fromGregorian 2021 11 9),
                                                      Just (fromGregorian 2023 01 12)]
        pacLatest <- testLog $ readPoseidonPackageCollection (testPacReadOpts {_readOptOnlyLatest = True}) [dir]
        sort (map getPacName pacLatest) `shouldBe` ["Lamnidis_2018", "Schiffels_2016", "Schmid_2028", "Wang_2020"]

files :: [String]
files  = ["test/testDat/testPackages/ancient/Schiffels_2016/geno.txt",
          "test/testDat/testPackages/ancient/Schiffels_2016/snp.txt",
          "test/testDat/testPackages/ancient/Schiffels_2016/ind.txt",
          "test/testDat/testPackages/ancient/Schiffels_2016/Schiffels_2016.janno",
          "test/testDat/testPackages/ancient/Schiffels_2016/sources.bib"]

checksums :: [String]
checksums = ["0332344057c0c4dce2ff7176f8e1103d",
             "d76e3e7a8fc0f1f5e435395424b5aeab",
             "f77dc756666dbfef3bb35191ae15a167",
             "09e65688bbb0d315648ccc7de0bf03e8",
             "59f4419dd96989c6185823e93f1aee0a"]

testGetChecksum :: Spec
testGetChecksum = describe "Poseidon.Package.getChecksum" $ do
    it "should determine checksums correctly" $ do
        mapM_ (\(f, c) -> do
            c_real <- getChecksum f
            c_real `shouldBe` c)
            (zip files checksums)

testRenderMismatch :: Spec
testRenderMismatch =
    describe "Poseidon.CLI.Validate.renderMismatch" $ do
    it "should not find mismatch for equal one-element lists" $ do
        renderMismatch ["a"] ["a"] `shouldBe` ""
    it "should find mismatch for non-equal one-element lists" $ do
        renderMismatch ["a"] ["b"] `shouldBe` "(a = b)"
    it "should not find mismatch for equal two-element lists" $ do
        renderMismatch ["a", "b"] ["a", "b"] `shouldBe` ""
    it "should find mismatch for non-equal two-element lists" $ do
        renderMismatch ["a", "b"] ["a", "c"] `shouldBe` "(b = c)"
    it "should stop printing at ten mismatches" $ do
        renderMismatch
            ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k"]
            ["b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"]
            `shouldBe`
            "(a = b), (b = c), (c = d), (d = e), (e = f), ..."
    it "should fill missing values with ?" $ do
        renderMismatch
            ["a", "b", "c"]
            ["d"]
            `shouldBe`
            "(a = d), (b = ?), (c = ?)"

testZipWithPadding :: Spec
testZipWithPadding = describe "Poseidon.CLI.Validate.zipWithPadding" $ do
    it "should zip normally for lists of equal length" $
        zwp ["a", "b"] ["c", "d"] `shouldBe` [("a", "c"), ("b", "d")]
    it "should fill for empty lists" $
        zwp ["a"] [] `shouldBe` [("a", "!")]
    it "should fill empty elements right" $
        zwp ["a", "b"] ["c"] `shouldBe` [("a", "c"), ("b", "!")]
    it "should fill empty elements left" $
        zwp ["a"] ["b", "c"] `shouldBe` [("a", "b"), ("?", "c")]
  where
    zwp = zipWithPadding ("?" :: String) ("!" :: String)

testGetJoinGenotypeData :: Spec
testGetJoinGenotypeData = describe "Poseidon.Package.getJointGenotypeData" $ do
    let pacFiles = ["test/testDat/testPackages/ancient/Lamnidis_2018/POSEIDON.yml",
                    "test/testDat/testPackages/ancient/Schiffels_2016/POSEIDON.yml"]
    it "should correctly load genotype data without intersect" $ do
        pacs <- testLog $ mapM (readPoseidonPackage testPacReadOpts) pacFiles
        jointDat <- runSafeT $ do
            (_, jointProd) <- getJointGenotypeData noLog False PlinkPopNameAsFamily pacs Nothing
            P.toListM jointProd
        length jointDat `shouldBe` 10
        jointDat !! 3 `shouldBe` (EigenstratSnpEntry (Chrom "1") 903426 0.024457 "1_903426" 'C' 'T',
                                  V.fromList $ [Het, Het, HomAlt, Het, HomRef, HomRef, Het, HomRef, HomRef, HomAlt] ++ replicate 10 Missing)
        jointDat !! 5 `shouldBe` (EigenstratSnpEntry (Chrom "2") 1018704 0.026288 "2_1018704" 'A' 'G',
                                  V.fromList $ replicate 10 Missing ++ [Het, Het, HomRef, Het, Missing, HomAlt, Het, HomRef, HomAlt, Het])
    it "should correctly load genotype data with intersect" $ do
        pacs <- testLog $ mapM (readPoseidonPackage testPacReadOpts) pacFiles
        jointDat <- runSafeT $ do
            (_, jointProd) <- getJointGenotypeData noLog True PlinkPopNameAsFamily pacs Nothing
            P.toListM jointProd
        length jointDat `shouldBe` 8
        jointDat !! 3 `shouldBe` (EigenstratSnpEntry (Chrom "1") 949654 0.025727 "1_949654" 'A' 'G',
                                  V.fromList [HomAlt, Het, Het, HomAlt, Het, HomAlt, HomAlt, HomAlt, HomAlt, HomAlt,
                                              HomAlt, Het, Het, HomAlt, Het, HomAlt, HomAlt, HomAlt, HomAlt, HomAlt])
        jointDat !! 4 `shouldBe` (EigenstratSnpEntry (Chrom "2") 1045331 0.026665 "2_1045331" 'G' 'A', V.fromList $ replicate 20 HomRef)
    it "should correctly load the right nr of SNPs with snpFile and no intersect" $ do
        pacs <- testLog $ mapM (readPoseidonPackage testPacReadOpts) pacFiles
        jointDat <- runSafeT $ do
            (_, jointProd) <- getJointGenotypeData noLog False PlinkPopNameAsFamily pacs (Just "test/testDat/snpFile.snp")
            P.toListM jointProd
        length jointDat `shouldBe` 6
    it "should correctly load the right nr of SNPs with snpFile and intersect" $ do
        pacs <- testLog $ mapM (readPoseidonPackage testPacReadOpts) pacFiles
        jointDat <- runSafeT $ do
            (_, jointProd) <- getJointGenotypeData noLog True PlinkPopNameAsFamily pacs (Just "test/testDat/snpFile.snp")
            P.toListM jointProd
        length jointDat `shouldBe` 4
    it "should fail with unordered SNP input file" $ do
        pacs <- testLog $ mapM (readPoseidonPackage testPacReadOpts) pacFiles
        let makeJointDat = runSafeT $ do
                (_, jointProd) <- getJointGenotypeData noLog False PlinkPopNameAsFamily pacs (Just "test/testDat/snpFile_unordered.snp")
                P.toListM jointProd
        makeJointDat `shouldThrow` isInputOrderException
    it "should skip incongruent alleles" $ do
        let pacFiles2 = ["test/testDat/testPackages/ancient/Lamnidis_2018/POSEIDON.yml",
                         "test/testDat/testPackages/test_incongruent_snps/POSEIDON.yml"]
        pacs <- testLog $ mapM (readPoseidonPackage testPacReadOpts) pacFiles2
        jointDat <- runSafeT $ do
            (_, jointProd) <- getJointGenotypeData noLog False PlinkPopNameAsFamily pacs Nothing
            P.toListM jointProd
        length jointDat `shouldBe` 7
  where
    isInputOrderException :: Selector WrongInputOrderException
    isInputOrderException (WrongInputOrderException _) = True

testThrowOnRead :: Spec
testThrowOnRead = describe "Poseidon.Package.readPoseidonPackage" $ do
    it "should throw if bibentries aren't found" $ do
        let opts = defaultPackageReadOptions {_readOptGenoCheck = False}
        let ymlPath = "test/testDat/testPackages/ancient/Lamnidis_2018/POSEIDON_nobib.yml"
        testLog (readPoseidonPackage opts ymlPath) `shouldThrow` isPoseidonCrossFileConsistencyException
    it "should throw if Plink Setting is not correct" $ do
        let opts = defaultPackageReadOptions
        let ymlPath = "test/testDat/testPackages/ancient/Wang_2020/POSEIDON.yml"
        usePoseidonLogger NoLog Testing PlinkPopNameAsPhenotype (readPoseidonPackage opts ymlPath) `shouldThrow` isPoseidonCrossFileConsistencyException
    it "should not throw if Plink Setting is correct" $ do
        let opts = defaultPackageReadOptions
        let ymlPath = "test/testDat/testPackages/ancient/Wang_2020/POSEIDON_otherPlinkEncoding.yml"
        _ <- usePoseidonLogger NoLog Testing PlinkPopNameAsPhenotype (readPoseidonPackage opts ymlPath)
        return ()
  where
    isPoseidonCrossFileConsistencyException :: Selector PoseidonException
    isPoseidonCrossFileConsistencyException (PoseidonCrossFileConsistencyException _ _) = True
    isPoseidonCrossFileConsistencyException _ = error "should never happen" -- just to make the linter happy
