{-# LANGUAGE OverloadedStrings #-}
module Poseidon.GenotypeDataSpec (spec) where

import           Poseidon.GenotypeData
import           Poseidon.Utils             (PoseidonException (..), noLog,
                                             testLog)

import           Data.List                  (isPrefixOf)
import qualified Data.Vector                as V
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (EigenstratIndEntry),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine, Sex (..))
import           SequenceFormats.Utils      (Chrom (..))
import           Test.Hspec

spec :: Spec
spec = do
    testSNPSetMergeList
    testJoinGenoEntries
    testLoadVCF

testSNPSetMergeList :: Spec
testSNPSetMergeList =
    describe "Poseidon.GenotypeData.snpSetMergeList" $ do
        it "should merge a list of length 1 correctly" $
            snpSetMergeList [SNPSet1240K] True `shouldBe` SNPSet1240K
        it "should merge a list with identical values correctly" $
            snpSetMergeList [SNPSet1240K, SNPSet1240K, SNPSet1240K] True `shouldBe` SNPSet1240K
        it "should merge with union merge correctly" $
            snpSetMergeList [SNPSet1240K, SNPSetHumanOrigins] False `shouldBe` SNPSet1240K
        it "should merge with intersect merge correctly" $
            snpSetMergeList [SNPSet1240K, SNPSetHumanOrigins] True `shouldBe` SNPSetHumanOrigins
        it "should merge with the snp set Other correctly" $
            snpSetMergeList [SNPSetOther, SNPSetHumanOrigins, SNPSet1240K] True `shouldBe` SNPSetOther

testEntriesTuplesList1 :: [Maybe (EigenstratSnpEntry, GenoLine)]
testEntriesTuplesList1 = [
    Just (EigenstratSnpEntry (Chrom "1") 1 0.1 "id1" 'A' 'C', V.fromList [HomRef, Het, HomAlt]),
    Nothing,
    Just (EigenstratSnpEntry (Chrom "1") 1 0.1 "id1" 'C' 'A', V.fromList [HomAlt, Missing, HomRef]),
    Just (EigenstratSnpEntry (Chrom "1") 1 0.1 "id1" 'C' 'N', V.fromList [HomRef, Missing, HomRef]),
    Just (EigenstratSnpEntry (Chrom "1") 1 0.1 "id1" 'N' 'A', V.fromList [HomAlt, HomAlt, HomAlt])]

mergedTestEntries1 :: (EigenstratSnpEntry, GenoLine)
mergedTestEntries1 = (
    EigenstratSnpEntry (Chrom "1") 1 0.1 "id1" 'A' 'C',
    V.fromList [HomRef, Het, HomAlt,
                Missing, Missing, Missing,
                HomRef, Missing, HomAlt,
                HomAlt, Missing, HomAlt,
                HomRef, HomRef, HomRef])

testJoinGenoEntries :: Spec
testJoinGenoEntries =
    describe "Poseidon.GenotypeData.joinEntries" $
        it "should just work" $ do
            let nrInds = [3, 3, 3, 3, 3]
                pacNames = ["Pac1", "Pac2", "Pac3", "Pac4", "Pac5"]
            joinEntries noLog nrInds pacNames testEntriesTuplesList1 `shouldReturn` mergedTestEntries1

testLoadVCF :: Spec
testLoadVCF = describe "loadIndividuals(VCF)" $ do
    it "should correctly read group names and genetic sex from VCF header" $ do
        let gSpec = GenotypeDataSpec (GenotypeVCF "geno.vcf" Nothing) Nothing
        let baseDir = "test/testDat/testPackages/other_test_packages/Schiffels_2016_vcf/"
        fmap (take 3) (testLog $ loadIndividuals baseDir gSpec) `shouldReturn`
            [EigenstratIndEntry "XXX001" Male "POP1", EigenstratIndEntry "XXX002" Female "POP2", EigenstratIndEntry "XXX003" Male "POP1"]
    it "should throw if encountering wrong number of group names" $ do
        let gSpec = GenotypeDataSpec (GenotypeVCF "geno_wrong_groupnames.vcf" Nothing) Nothing
        let baseDir = "test/testDat/testGenoFiles"
        testLog (loadIndividuals baseDir gSpec) `shouldThrow` groupNameExc
    it "should throw if encountering wrong number of genetic sex entries" $ do
        let gSpec = GenotypeDataSpec (GenotypeVCF "geno_wrong_sexEntries.vcf" Nothing) Nothing
        let baseDir = "test/testDat/testGenoFiles"
        testLog (loadIndividuals baseDir gSpec) `shouldThrow` sexEntryExc
  where
    groupNameExc (PoseidonGenotypeException msg) = "Number of group names" `isPrefixOf` msg
    groupNameExc _ = False
    sexEntryExc (PoseidonGenotypeException msg) = "Number of genetic sex" `isPrefixOf` msg
    sexEntryExc _ = False
