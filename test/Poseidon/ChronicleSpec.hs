{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.ChronicleSpec (spec) where

import           Poseidon.Chronicle    (ChronicleMode (..), PackageState (..),
                                        PoseidonPackageChronicle (..),
                                        makeChronicle, readChronicle,
                                        updateChronicle, writeChronicle)
import           Poseidon.Package      (PackageReadOptions (..),
                                        defaultPackageReadOptions,
                                        dummyContributor,
                                        readPoseidonPackageCollection)
import           Poseidon.Utils        (testLog)

import qualified Data.ByteString.Char8 as B
import           Data.Either           (fromRight)
import           Data.Time             (fromGregorian)
import           Data.Version          (makeVersion)
import           Data.Yaml             (ParseException, decodeEither')
import           System.Directory      (removeFile)
import           Test.Hspec
import           Text.RawString.QQ

spec :: Spec
spec = do
    testChronicleFromYaml
    testEncodeDecodeChronicleFile
    testMakeChronicle
    testUpdateChronicle

yamlExampleChronicle :: B.ByteString
yamlExampleChronicle = [r|
title: Chronicle title
description: Chronicle description
contributor:
- name: Josiah Carberry
  email: carberry@brown.edu
  orcid: 0000-0002-1825-0097
chronicleVersion: 0.1.0
lastModified: 2023-04-02
packages:
- title: Lamnidis_2018
  version: 1.0.1
- title: Schiffels_2016
  version: 1.0.1
- title: Schmid_2028
  version: 1.0.0
- title: Wang_2020
  version: 0.1.0
|]

exampleChronicle :: PoseidonPackageChronicle
exampleChronicle = PoseidonPackageChronicle {
      snapYamlTitle           = Just "Chronicle title"
    , snapYamlDescription     = Just "Chronicle description"
    , snapYamlContributor     = [dummyContributor]
    , snapYamlChronicleVersion = Just $ makeVersion [0, 1, 0]
    , snapYamlLastModified    = Just (fromGregorian 2023 04 02)
    , snapYamlPackages        = [
        PackageState {
              pacStateTitle   = "Lamnidis_2018"
            , pacStateVersion = Just $ makeVersion [1, 0, 1]
            , pacStateCommit  = Nothing
        },
        PackageState {
              pacStateTitle   = "Schiffels_2016"
            , pacStateVersion = Just $ makeVersion [1, 0, 1]
            , pacStateCommit  = Nothing
        },
        PackageState {
              pacStateTitle   = "Schmid_2028"
            , pacStateVersion = Just $ makeVersion [1, 0, 0]
            , pacStateCommit  = Nothing
        },
        PackageState {
              pacStateTitle   = "Wang_2020"
            , pacStateVersion = Just $ makeVersion [0, 1, 0]
            , pacStateCommit  = Nothing
        }
        ]
    }

newChronicle :: PoseidonPackageChronicle
newChronicle = PoseidonPackageChronicle {
      snapYamlTitle           = Nothing
    , snapYamlDescription     = Nothing
    , snapYamlContributor     = []
    , snapYamlChronicleVersion = Nothing
    , snapYamlLastModified    = Just (fromGregorian 2099 04 02)
    , snapYamlPackages        = [
        PackageState {
              pacStateTitle   = "Lamnidis_2018"
            , pacStateVersion = Just $ makeVersion [2, 0, 0]
            , pacStateCommit  = Just "test"
        },
        PackageState {
              pacStateTitle   = "Zoro_2000"
            , pacStateVersion = Just $ makeVersion [0, 1, 0]
            , pacStateCommit  = Just "test2"
        }
        ]
    }

testPacReadOpts :: PackageReadOptions
testPacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    }

testChronicleFromYaml :: Spec
testChronicleFromYaml = describe "Poseidon.Chronicle.fromYAML" $ do
    let p = fromRight newChronicle (decodeEither' yamlExampleChronicle :: Either ParseException PoseidonPackageChronicle)
    it "should parse correct YAML data" $
        p `shouldBe` exampleChronicle

testEncodeDecodeChronicleFile :: Spec
testEncodeDecodeChronicleFile = describe "Poseidon.Chronicle.writeChronicle+readChronicle" $ do
    let tmpFile = "/tmp/poseidonTestChronicleFile"
    it "should write and read again correctly" $ do
        testLog $ writeChronicle tmpFile exampleChronicle
        res <- testLog $ readChronicle tmpFile
        removeFile tmpFile
        res `shouldBe` exampleChronicle

testMakeChronicle :: Spec
testMakeChronicle = describe "Poseidon.Chronicle.makeChronicle" $ do
    it "should make a chronicle as expected" $ do
        pacs <- testLog $ readPoseidonPackageCollection testPacReadOpts ["test/testDat/testPackages/ancient"]
        snap <- testLog $ makeChronicle SimpleChronicle pacs
        snap {snapYamlLastModified = Just (fromGregorian 2023 04 02)} `shouldBe` exampleChronicle

testUpdateChronicle :: Spec
testUpdateChronicle = describe "Poseidon.Chronicle.updateChronicle" $ do
    it "should correctly produce a new, merged chronicle" $ do
        updateChronicle exampleChronicle newChronicle `shouldBe`
            PoseidonPackageChronicle {
                  snapYamlTitle           = Just "Chronicle title"
                , snapYamlDescription     = Just "Chronicle description"
                , snapYamlContributor     = [dummyContributor]
                , snapYamlChronicleVersion = Just $ makeVersion [0, 2, 0]
                , snapYamlLastModified    = Just (fromGregorian 2099 04 02)
                , snapYamlPackages        = [
                    PackageState {
                          pacStateTitle   = "Lamnidis_2018"
                        , pacStateVersion = Just $ makeVersion [1, 0, 1]
                        , pacStateCommit  = Nothing
                    },
                    PackageState {
                          pacStateTitle   = "Lamnidis_2018"
                        , pacStateVersion = Just $ makeVersion [2, 0, 0]
                        , pacStateCommit  = Just "test"
                    },
                    PackageState {
                          pacStateTitle   = "Schiffels_2016"
                        , pacStateVersion = Just $ makeVersion [1, 0, 1]
                        , pacStateCommit  = Nothing
                    },
                    PackageState {
                          pacStateTitle   = "Schmid_2028"
                        , pacStateVersion = Just $ makeVersion [1, 0, 0]
                        , pacStateCommit  = Nothing
                    },
                    PackageState {
                          pacStateTitle   = "Wang_2020"
                        , pacStateVersion = Just $ makeVersion [0, 1, 0]
                        , pacStateCommit  = Nothing
                    },
                    PackageState {
                          pacStateTitle   = "Zoro_2000"
                        , pacStateVersion = Just $ makeVersion [0, 1, 0]
                        , pacStateCommit  = Just "test2"
                    }
                    ]
                }
