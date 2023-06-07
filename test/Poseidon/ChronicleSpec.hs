{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.ChronicleSpec (spec) where

import           Poseidon.Chronicle    (PackageState (..),
                                        PoseidonPackageChronicle (..),
                                        makeChronicle, readChronicle,
                                        updateChronicle, writeChronicle)
import           Poseidon.Package      (PackageReadOptions (..),
                                        defaultPackageReadOptions,
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
chronicleVersion: 0.1.0
lastModified: 2023-04-02
packages:
- title: Lamnidis_2018
  version: 1.0.1
  commit: MyGitCommitHash
- title: Schiffels_2016
  version: 1.0.1
  commit: MyGitCommitHash
- title: Schmid_2028
  version: 1.0.0
  commit: MyGitCommitHash
- title: Wang_2020
  version: 0.1.0
  commit: MyGitCommitHash
|]

exampleChronicle :: PoseidonPackageChronicle
exampleChronicle = PoseidonPackageChronicle {
      snapYamlTitle           = "Chronicle title"
    , snapYamlDescription     = Just "Chronicle description"
    , snapYamlChronicleVersion = makeVersion [0, 1, 0]
    , snapYamlLastModified    = fromGregorian 2023 04 02
    , snapYamlPackages        = [
        PackageState {
              pacStateTitle   = "Lamnidis_2018"
            , pacStateVersion = makeVersion [1, 0, 1]
            , pacStateCommit  = "MyGitCommitHash"
        },
        PackageState {
              pacStateTitle   = "Schiffels_2016"
            , pacStateVersion = makeVersion [1, 0, 1]
            , pacStateCommit  = "MyGitCommitHash"
        },
        PackageState {
              pacStateTitle   = "Schmid_2028"
            , pacStateVersion = makeVersion [1, 0, 0]
            , pacStateCommit  = "MyGitCommitHash"
        },
        PackageState {
              pacStateTitle   = "Wang_2020"
            , pacStateVersion = makeVersion [0, 1, 0]
            , pacStateCommit  = "MyGitCommitHash"
        }
        ]
    }

newChronicle :: PoseidonPackageChronicle
newChronicle = PoseidonPackageChronicle {
      snapYamlTitle            = ""
    , snapYamlDescription      = Nothing
    , snapYamlChronicleVersion = makeVersion [1, 0, 0]
    , snapYamlLastModified     = fromGregorian 2099 04 02
    , snapYamlPackages         = [
        PackageState {
              pacStateTitle    = "Lamnidis_2018"
            , pacStateVersion  = makeVersion [2, 0, 0]
            , pacStateCommit   = "test"
        },
        PackageState {
              pacStateTitle    = "Zoro_2000"
            , pacStateVersion  = makeVersion [0, 1, 0]
            , pacStateCommit   = "test2"
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
        snap <- testLog $ makeChronicle True pacs
        snap {snapYamlLastModified = fromGregorian 2023 04 02} `shouldBe` exampleChronicle

testUpdateChronicle :: Spec
testUpdateChronicle = describe "Poseidon.Chronicle.updateChronicle" $ do
    it "should correctly produce a new, merged chronicle" $ do
        updateChronicle exampleChronicle newChronicle `shouldBe`
            PoseidonPackageChronicle {
                  snapYamlTitle            = "Chronicle title"
                , snapYamlDescription      = Just "Chronicle description"
                , snapYamlChronicleVersion = makeVersion [0, 2, 0]
                , snapYamlLastModified     = fromGregorian 2099 04 02
                , snapYamlPackages         = [
                    PackageState {
                          pacStateTitle    = "Lamnidis_2018"
                        , pacStateVersion  = makeVersion [1, 0, 1]
                        , pacStateCommit   = "MyGitCommitHash"
                    },
                    PackageState {
                          pacStateTitle    = "Lamnidis_2018"
                        , pacStateVersion  = makeVersion [2, 0, 0]
                        , pacStateCommit   = "test"
                    },
                    PackageState {
                          pacStateTitle    = "Schiffels_2016"
                        , pacStateVersion  = makeVersion [1, 0, 1]
                        , pacStateCommit   = "MyGitCommitHash"
                    },
                    PackageState {
                          pacStateTitle    = "Schmid_2028"
                        , pacStateVersion  = makeVersion [1, 0, 0]
                        , pacStateCommit   = "MyGitCommitHash"
                    },
                    PackageState {
                          pacStateTitle    = "Wang_2020"
                        , pacStateVersion  = makeVersion [0, 1, 0]
                        , pacStateCommit   = "MyGitCommitHash"
                    },
                    PackageState {
                          pacStateTitle    = "Zoro_2000"
                        , pacStateVersion  = makeVersion [0, 1, 0]
                        , pacStateCommit   = "test2"
                    }
                    ]
                }
