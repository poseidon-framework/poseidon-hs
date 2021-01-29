module Poseidon.ChecksumsSpec (spec) where

import           Poseidon.Package (getChecksum)

import Control.Monad (forM_)
import           Data.Maybe
import           Test.Hspec

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

spec = do
    testGetChecksum

testGetChecksum :: Spec
testGetChecksum = describe "Poseidon.Checksums.getChecksum" $ do
    it "should determine checksums correctly" $
        forM_ (zip files checksums) $ \(f, c) -> do
            chkSum <- getChecksum f
            chkSum `shouldBe` c
