module Poseidon.ChecksumsSpec (spec) where

import           Poseidon.Checksums

import           Data.Maybe
import           Test.Hspec

spec = do
    testGetChecksum
    testGetChecksumMaybe
    testMakeChecksumList

testGetChecksum :: Spec
testGetChecksum = describe "Poseidon.Checksums.getChecksum" $ do
    let genoPath = "test/testDat/testModules/ancient/Schiffels_2016/geno.txt"
    it "should determine checksums correctly" $ do
        checksum <- getChecksum genoPath
        checksum `shouldBe` "95b093eefacc1d6499afcfe89b15d56c"

testGetChecksumMaybe :: Spec
testGetChecksumMaybe = describe "Poseidon.Checksums.getChecksumMaybe" $ do
    let noPath = Nothing
        genoPath = "test/testDat/testModules/ancient/Schiffels_2016/geno.txt"
    it "should pass on nothing" $ do
        checksum <- getChecksumMaybe noPath
        checksum `shouldBe` Nothing
    it "should determine checksums correctly" $ do
        checksum <- getChecksumMaybe $ Just genoPath
        checksum `shouldBe` Just "95b093eefacc1d6499afcfe89b15d56c"

testMakeChecksumList :: Spec
testMakeChecksumList = describe "Poseidon.Checksums.makeChecksumList" $ do
    let noPath = Nothing
        genoPath  = Just "test/testDat/testModules/ancient/Schiffels_2016/geno.txt"
        snpPath   = Just "test/testDat/testModules/ancient/Schiffels_2016/snp.txt"
        indPath   = Just "test/testDat/testModules/ancient/Schiffels_2016/ind.txt"
        jannoPath = Just "test/testDat/testModules/ancient/Schiffels_2016/Schiffels_2016.janno"
        bibPath   = Just "test/testDat/testModules/ancient/Schiffels_2016/sources.bib"
        partialLoad = Just ChecksumListSpec { 
              genoFileCheck  = Just "95b093eefacc1d6499afcfe89b15d56c"
            , snpFileCheck   = Just "6771d7c873219039ba3d5bdd96031ce3"
            , indFileCheck   = Just "f77dc756666dbfef3bb35191ae15a167"
            , jannoFileCheck = Nothing
            , bibFileCheck   = Nothing
            }
        fullLoad = Just ChecksumListSpec { 
              genoFileCheck  = Just "95b093eefacc1d6499afcfe89b15d56c"
            , snpFileCheck   = Just "6771d7c873219039ba3d5bdd96031ce3"
            , indFileCheck   = Just "f77dc756666dbfef3bb35191ae15a167"
            , jannoFileCheck = Just "555d7733135ebcabd032d581381c5d6f"
            , bibFileCheck   = Just "70cd3d5801cee8a93fc2eb40a99c63fa"
            }
    it "should pass on nothing" $ do
        checksumList <- makeChecksumList noPath noPath noPath noPath noPath
        checksumList `shouldBe` Nothing
    it "should work on partial input" $ do
        checksumList <- makeChecksumList genoPath snpPath indPath noPath noPath
        checksumList `shouldBe` partialLoad
    it "should work on full input" $ do
        checksumList <- makeChecksumList genoPath snpPath indPath noPath noPath
        checksumList `shouldBe` fullLoad
