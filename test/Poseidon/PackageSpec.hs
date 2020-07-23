{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Poseidon.PackageSpec (spec) where

import           Poseidon.Package          (ContributorSpec (..),
                                            GenotypeDataSpec (..),
                                            GenotypeFormatSpec (..),
                                            PoseidonPackage (..),
                                            findPoseidonPackages,
                                            readPoseidonPackage, filterDuplicatePackages)

import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8     as B
import Data.List (sort)
import           Data.Time                 (defaultTimeLocale, fromGregorian,
                                            parseTimeOrError)
import           Data.Version              (makeVersion)
import           Data.Yaml                 (ParseException, decodeEither')
import           System.IO                 (IOMode (..), hPutStrLn, stderr,
                                            withFile)
import           Test.Hspec
import           Text.RawString.QQ

spec = do
    testPoseidonFromYAML
    testReadPoseidonYAML
    testFindPoseidonPackages

yamlPackage :: B.ByteString
yamlPackage = [r|
poseidonVersion: 2.0.1
title: Schiffels_2016
description: Genetic data published in Schiffels et al. 2016
contributor:
  name: Stephan Schiffels
  email: schiffels@institute.org
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

truePackage :: PoseidonPackage
truePackage = PoseidonPackage {
    posPacPoseidonVersion = makeVersion [2, 0, 1],
    posPacTitle = "Schiffels_2016",
    posPacDescription = "Genetic data published in Schiffels et al. 2016",
    posPacContributor = ContributorSpec "Stephan Schiffels" "schiffels@institute.org",
    posPacLastModified = fromGregorian 2020 2 28,
    posPacBibFile = Just "sources.bib",
    posPacGenotypeData = GenotypeDataSpec {
        format = GenotypeFormatPlink,
        genoFile = "Schiffels_2016.bed",
        snpFile = "Schiffels_2016.bim",
        indFile = "Schiffels_2016.fam"
    },
    posPacJannoFile = "Schiffels_2016.janno"
}

testPoseidonFromYAML :: Spec
testPoseidonFromYAML = describe "PoseidonPackage.fromYAML" $ do
    let (Right p) = decodeEither' yamlPackage :: Either ParseException PoseidonPackage
    it "should parse correct YAML data" $
        p `shouldBe` truePackage
    let yamlPackage2 = replace "2020-02-28" "2019-07-10sss" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonPackage
    it "should give error with incorrect date string" $ do
        show err `shouldBe` "AesonException \"Error in $.lastModified: could not parse date: endOfInput\""
    let yamlPackage2 = replace "2.0.1" "1.0.0sss" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonPackage
    it "should give error with incorrect version string" $ do
        show err `shouldBe` "AesonException \"Error in $.poseidonVersion: parsing Version failed\""
    let yamlPackage2 = replace "bibFile: sources.bib\n" "" yamlPackage
        (Right p) = decodeEither' yamlPackage2 :: Either ParseException PoseidonPackage
    it "should give Nothing for missing bibFile" $ do
        p `shouldBe` truePackage {posPacBibFile = Nothing}
    let yamlPackage2 = replace "title: Schiffels_2016\n" "" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonPackage
    it "should fail with title missing" $ do
        show err `shouldBe` "AesonException \"Error in $: key \\\"title\\\" not found\""
    let yamlPackage2 = replace "poseidonVersion: 2.0.1\n" "" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonPackage
    it "should fail with poseidonVersion missing" $ do
        show err `shouldBe` "AesonException \"Error in $: key \\\"poseidonVersion\\\" not found\""
    let yamlPackage2 = replace "lastModified: 2020-02-28\n" "" yamlPackage
        (Left err) = decodeEither' yamlPackage2 :: Either ParseException PoseidonPackage
    it "should fail with lastModified missing" $ do
        show err `shouldBe` "AesonException \"Error in $: key \\\"lastModified\\\" not found\""

testReadPoseidonYAML :: Spec
testReadPoseidonYAML = describe "PoseidonPackage.readPoseidonPackage" $ do
    let fn = "/tmp/poseidon_test.yml"
    it "should return correct package from file read" $ do
        withFile fn WriteMode $ \h -> B.hPutStr h yamlPackage
        pac <- readPoseidonPackage fn
        posPacBibFile pac `shouldBe` Just "/tmp/sources.bib"
        (genoFile . posPacGenotypeData) pac `shouldBe` "/tmp/Schiffels_2016.bed"

testFindPoseidonPackages :: Spec
testFindPoseidonPackages = describe "PoseidonPackage.findPoseidonPackages" $ do
    let dir = "test/testDat/testModules/ancient"
    it "should discover packages correctly" $ do
        pac <- findPoseidonPackages dir
        sort (map posPacTitle pac) `shouldBe` ["Lamnidis_2018", "Lamnidis_2018", "Schiffels_2016"]
    it "should handle duplicate names correctly" $ do
        pac <- fmap filterDuplicatePackages . findPoseidonPackages $ dir
        sort (map posPacTitle pac) `shouldBe` ["Lamnidis_2018", "Schiffels_2016"]
        sort (map posPacLastModified pac) `shouldBe` [fromGregorian 2020 2 20, fromGregorian 2020 2 28]
        
