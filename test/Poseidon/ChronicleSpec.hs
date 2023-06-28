{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.ChronicleSpec (spec) where

import           Poseidon.Chronicle    (PackageIteration (..),
                                        PoseidonPackageChronicle (..),
                                        makeChronicle, readChronicle,
                                        updateChronicle, writeChronicle)
import           Poseidon.Package      (PackageReadOptions (..),
                                        defaultPackageReadOptions,
                                        readPoseidonPackageCollection)
import           Poseidon.Utils        (testLog)

import qualified Data.ByteString.Char8 as B
import           Data.Either           (fromRight)
import qualified Data.Set              as S
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
  path: ./Lamnidis_2018
- title: Schiffels_2016
  version: 1.0.1
  commit: MyGitCommitHash
  path: ./Schiffels_2016
- title: Schmid_2028
  version: 1.0.0
  commit: MyGitCommitHash
  path: ./Schmid_2028
- title: Wang_2020
  version: 0.1.0
  commit: MyGitCommitHash
  path: ./Wang_2020
|]

exampleChronicle :: PoseidonPackageChronicle
exampleChronicle = PoseidonPackageChronicle {
      snapYamlTitle           = "Chronicle title"
    , snapYamlDescription     = Just "Chronicle description"
    , snapYamlChronicleVersion = makeVersion [0, 1, 0]
    , snapYamlLastModified    = fromGregorian 2023 04 02
    , snapYamlPackages        = S.fromList [
        PackageIteration {
              pacStateTitle   = "Lamnidis_2018"
            , pacStateVersion = makeVersion [1, 0, 1]
            , pacStateCommit  = "MyGitCommitHash"
            , pacStatePath    = "./Lamnidis_2018"
        },
        PackageIteration {
              pacStateTitle   = "Schiffels_2016"
            , pacStateVersion = makeVersion [1, 0, 1]
            , pacStateCommit  = "MyGitCommitHash"
            , pacStatePath    = "./Schiffels_2016"
        },
        PackageIteration {
              pacStateTitle   = "Schmid_2028"
            , pacStateVersion = makeVersion [1, 0, 0]
            , pacStateCommit  = "MyGitCommitHash"
            , pacStatePath    = "./Schmid_2028"
        },
        PackageIteration {
              pacStateTitle   = "Wang_2020"
            , pacStateVersion = makeVersion [0, 1, 0]
            , pacStateCommit  = "MyGitCommitHash"
            , pacStatePath    = "./Wang_2020"
        }
        ]
    }

newChronicle :: PoseidonPackageChronicle
newChronicle = PoseidonPackageChronicle {
      snapYamlTitle            = ""
    , snapYamlDescription      = Nothing
    , snapYamlChronicleVersion = makeVersion [1, 0, 0]
    , snapYamlLastModified     = fromGregorian 2099 04 02
    , snapYamlPackages         = S.fromList [
        PackageIteration {
              pacStateTitle    = "Lamnidis_2018"
            , pacStateVersion  = makeVersion [2, 0, 0]
            , pacStateCommit   = "test"
            , pacStatePath    = ""
        },
        PackageIteration {
              pacStateTitle    = "Zoro_2000"
            , pacStateVersion  = makeVersion [0, 1, 0]
            , pacStateCommit   = "test2"
            , pacStatePath    = ""
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
        snap <- testLog $ makeChronicle True "test/testDat/testPackages/ancient" pacs
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
                , snapYamlPackages         = S.fromList [
                    PackageIteration {
                          pacStateTitle    = "Lamnidis_2018"
                        , pacStateVersion  = makeVersion [1, 0, 1]
                        , pacStateCommit   = "MyGitCommitHash"
                        , pacStatePath     = ""
                    },
                    PackageIteration {
                          pacStateTitle    = "Lamnidis_2018"
                        , pacStateVersion  = makeVersion [2, 0, 0]
                        , pacStateCommit   = "test"
                        , pacStatePath     = ""
                    },
                    PackageIteration {
                          pacStateTitle    = "Schiffels_2016"
                        , pacStateVersion  = makeVersion [1, 0, 1]
                        , pacStateCommit   = "MyGitCommitHash"
                        , pacStatePath     = ""
                    },
                    PackageIteration {
                          pacStateTitle    = "Schmid_2028"
                        , pacStateVersion  = makeVersion [1, 0, 0]
                        , pacStateCommit   = "MyGitCommitHash"
                        , pacStatePath     = ""
                    },
                    PackageIteration {
                          pacStateTitle    = "Wang_2020"
                        , pacStateVersion  = makeVersion [0, 1, 0]
                        , pacStateCommit   = "MyGitCommitHash"
                        , pacStatePath     = ""
                    },
                    PackageIteration {
                          pacStateTitle    = "Zoro_2000"
                        , pacStateVersion  = makeVersion [0, 1, 0]
                        , pacStateCommit   = "test2"
                        , pacStatePath     = ""
                    }
                    ]
                }
