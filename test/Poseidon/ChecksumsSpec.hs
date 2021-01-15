module Poseidon.ChecksumsSpec (spec) where

import           Poseidon.Checksums

import           Test.Hspec

spec = do
    testGetChecksum

testGetChecksum :: Spec
testGetChecksum = describe "Poseidon.Checksums.getChecksum" $ do
    let genoPath = "test/testDat/testModules/ancient/Schiffels_2016/geno.txt"
    it "should determine checksums correctly" $ do
        checksum <- getChecksum genoPath
        checksum `shouldBe` "95b093eefacc1d6499afcfe89b15d56c"
