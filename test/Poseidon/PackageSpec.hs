{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Poseidon.PackageSpec (spec) where

import           Poseidon.GenotypeData     (GenotypeDataSpec (..),
                                            GenotypeFormatSpec (..))
import           Poseidon.Package          (ContributorSpec (..),
                                            PoseidonYamlStruct (..),
                                            PoseidonPackage (..),
                                            filterDuplicatePackages,
                                            readPoseidonPackageCollection,
                                            renderMismatch,
                                            zipWithPadding,
                                            getChecksum)

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as B
import           Data.List                 (sort)
import           Data.Time                 (defaultTimeLocale, fromGregorian,
                                            parseTimeOrError)
import           Data.Version              (makeVersion)
import           Data.Yaml                 (ParseException, decodeEither',
                                            encodeFile)
import           System.IO                 (IOMode (..), hPutStrLn, stderr,
                                            withFile)
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testPoseidonFromYAML
    testreadPoseidonPackageCollection
    testGetChecksum
    testRenderMismatch
    testZipWithPadding

yamlTestPath :: FilePath
yamlTestPath = "/tmp/poseidon_test.yml"

yamlPackage :: B.ByteString
yamlPackage = [r|
poseidonVersion: 2.0.1
title: Schiffels_2016
description: Genetic data published in Schiffels et al. 2016
contributor:
  - name: Stephan Schiffels
    email: schiffels@institute.org
packageVersion: 1.0.0
lastModified: 2020-02-28
bibFile: sources.bib
genotypeData:
  format: PLINK
  genoFile: Schiffels_2016.bed
  snpFile: Schiffels_2016.bim
  indFile: Schiffels_2016.fam
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
    _posYamlContributor     = [ContributorSpec "Stephan Schiffels" "schiffels@institute.org"],
    _posYamlPackageVersion  = Just $ makeVersion [1, 0, 0],
    _posYamlLastModified    = Just $ fromGregorian 2020 2 28,
    _posYamlGenotypeData    = GenotypeDataSpec {
        format   = GenotypeFormatPlink,
        genoFile = "Schiffels_2016.bed",
        genoFileChkSum = Nothing,
        snpFile  = "Schiffels_2016.bim",
        snpFileChkSum = Nothing,
        indFile  = "Schiffels_2016.fam",
        indFileChkSum = Nothing
    },
    _posYamlJannoFile       = Just "Schiffels_2016.janno",
    _posYamlJannoFileChkSum = Nothing,
    _posYamlBibFile         = Just "sources.bib",
    _posYamlBibFileChkSum   = Nothing
}

truePackageAbsPaths :: PoseidonYamlStruct
truePackageAbsPaths = PoseidonYamlStruct {
    _posYamlPoseidonVersion = makeVersion [2, 0, 1],
    _posYamlTitle           = "Schiffels_2016",
    _posYamlDescription     = Just "Genetic data published in Schiffels et al. 2016",
    _posYamlContributor     = [ContributorSpec "Stephan Schiffels" "schiffels@institute.org"],
    _posYamlPackageVersion  = Just $ makeVersion [1, 0, 0],
    _posYamlLastModified    = Just $ fromGregorian 2020 2 28,
    _posYamlGenotypeData    = GenotypeDataSpec {
        format   = GenotypeFormatPlink,
        genoFile = "/tmp/Schiffels_2016.bed",
        genoFileChkSum = Nothing,
        snpFile  = "/tmp/Schiffels_2016.bim",
        snpFileChkSum = Nothing,
        indFile  = "/tmp/Schiffels_2016.fam",
        indFileChkSum = Nothing
    },
    _posYamlJannoFile       = Just "/tmp/Schiffels_2016.janno",
    _posYamlJannoFileChkSum = Nothing,
    _posYamlBibFile         = Just "/tmp/sources.bib",
    _posYamlBibFileChkSum   = Nothing
}

testPoseidonFromYAML :: Spec
testPoseidonFromYAML = describe "PoseidonPackage.fromYAML" $ do
    let (Right p) = decodeEither' yamlPackage :: Either ParseException PoseidonYamlStruct
    it "should parse correct YAML data" $
        p `shouldBe` truePackageRelPaths
    let yamlPackage2 = replace "2020-02-28" "2019-07-10sss" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should give error with incorrect date string" $ do
        show err `shouldBe` "AesonException \"Error in $.lastModified: could not parse date: endOfInput\""
    let yamlPackage2 = replace "2.0.1" "1.0.0sss" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should give error with incorrect poseidonVersion string" $ do
        show err `shouldBe` "AesonException \"Error in $.poseidonVersion: parsing Version failed\""
    let yamlPackage2 = replace "1.0.0" "a.b.c" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should give error with incorrect packageVersion string" $ do
        show err `shouldBe` "AesonException \"Error in $.packageVersion: parsing Version failed\""
    let yamlPackage2 = replace "bibFile: sources.bib\n" "" yamlPackage
        (Right p) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should give Nothing for missing bibFile" $ do
        p `shouldBe` truePackageRelPaths {_posYamlBibFile = Nothing}
    let yamlPackage2 = replace "title: Schiffels_2016\n" "" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should fail with title missing" $ do
        show err `shouldBe` "AesonException \"Error in $: key \\\"title\\\" not found\""
    let yamlPackage2 = replace "poseidonVersion: 2.0.1\n" "" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should fail with poseidonVersion missing" $ do
        show err `shouldBe` "AesonException \"Error in $: key \\\"poseidonVersion\\\" not found\""
    let yamlPackage2 = replace "lastModified: 2020-02-28\n" "" yamlPackage
        (Right p) = decodeEither' yamlPackage2 :: Either ParseException PoseidonYamlStruct
    it "should fail with lastModified missing" $ do
        p `shouldBe` truePackageRelPaths {_posYamlLastModified = Nothing}

testreadPoseidonPackageCollection :: Spec
testreadPoseidonPackageCollection = describe "PoseidonPackage.findPoseidonPackages" $ do
    let dir = "test/testDat/testModules/ancient"
    it "should discover packages correctly" $ do
        pac <- readPoseidonPackageCollection True False False [dir]
        sort (map posPacTitle pac) `shouldBe` ["Lamnidis_2018", "Schiffels_2016", "Wang_Plink_test_2020"]
        sort (map posPacLastModified pac) `shouldBe` [Just (fromGregorian 2020 2 20),
                                                      Just (fromGregorian 2020 2 28),
                                                      Just (fromGregorian 2020 05 20)]

files  = ["test/testDat/testModules/ancient/Schiffels_2016/geno.txt",
          "test/testDat/testModules/ancient/Schiffels_2016/snp.txt",
          "test/testDat/testModules/ancient/Schiffels_2016/ind.txt",
          "test/testDat/testModules/ancient/Schiffels_2016/Schiffels_2016.janno",
          "test/testDat/testModules/ancient/Schiffels_2016/sources.bib"]

checksums = ["95b093eefacc1d6499afcfe89b15d56c",
             "6771d7c873219039ba3d5bdd96031ce3",
             "f77dc756666dbfef3bb35191ae15a167",
             "555d7733135ebcabd032d581381c5d6f",
             "70cd3d5801cee8a93fc2eb40a99c63fa"]

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
testZipWithPadding = 
    describe "Poseidon.CLI.Validate.zipWithPadding" $ do
    it "should zip normally for lists of equal length" $ do
        zipWithPadding "?" "!" ["a", "b"] ["c", "d"] `shouldBe` [("a", "c"), ("b", "d")]
    it "should fill for empty lists" $ do
        zipWithPadding "?" "!" ["a"] [] `shouldBe` [("a", "!")]
    it "should fill empty elements right" $ do
        zipWithPadding "?" "!" ["a", "b"] ["c"] `shouldBe` [("a", "c"), ("b", "!")]
    it "should fill empty elements left" $ do
        zipWithPadding "?" "!" ["a"] ["b", "c"] `shouldBe` [("a", "b"), ("?", "c")]