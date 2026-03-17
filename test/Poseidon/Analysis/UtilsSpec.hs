module Poseidon.Analysis.UtilsSpec (spec) where

import           Poseidon.Analysis.Utils    (addGroupDefs, computeAlleleCount)

import qualified Data.Vector                as V
import           Poseidon.ColumnTypesJanno  (JannoGenotypePloidy (..))
import           Poseidon.EntityTypes       (IndividualInfo (..),
                                             PoseidonEntity (..),
                                             SignedEntity (..))
import           Poseidon.Package           (PackageReadOptions (..),
                                             defaultPackageReadOptions,
                                             getJointIndividualInfo,
                                             readPoseidonPackageCollection)
import           Poseidon.Utils             (PoseidonException (..), testLog)
import           SequenceFormats.Eigenstrat (GenoEntry (..))
import           Test.Hspec

spec :: Spec
spec = do
    testAddGroupDefs
    testComputeAlleleCount

testPacReadOpts :: PackageReadOptions
testPacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums  = False
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = False
    , _readOptOnlyLatest       = True
    }

testAddGroupDefs :: Spec
testAddGroupDefs = do
    describe "addGroupDefs" $ do
        it "should correctly add groups" $ do
            pacs <- testLog $ readPoseidonPackageCollection testPacReadOpts ["test/testPackages"]
            let groupDefs = [
                    ("MyNewGroup1", [Include (Group "POP1"), Exclude (Ind "XXX011")]),
                    ("MyNewGroup2", [Include (Ind "XXX011"), Include (Ind "XXX012")])
                    ]
            let newPacs = addGroupDefs groupDefs pacs
            (indInfo, _) <- getJointIndividualInfo newPacs
            (map indInfoGroups . filter ((=="XXX017") . indInfoName) $ indInfo) `shouldBe` [["POP1", "MyNewGroup1"]]
            (map indInfoGroups . filter ((=="XXX007") . indInfoName) $ indInfo) `shouldBe` [["POP1", "MyNewGroup1"]]
            (map indInfoGroups . filter ((=="XXX011") . indInfoName) $ indInfo) `shouldBe` [["POP1", "MyNewGroup2"]]
            (map indInfoGroups . filter ((=="XXX012") . indInfoName) $ indInfo) `shouldBe` [["POP2", "MyNewGroup2"]]

testComputeAlleleCount :: Spec
testComputeAlleleCount =
    describe "computeAlleleCount" $ do
        let indivNames = ["Ind" ++ show i | i <- [(0 :: Int)..4]]
        it "should correctly sum up alleles in case of diploids/haploids" $ do
            let ploidyVec = V.fromList [Haploid, Haploid, Haploid, Diploid, Diploid]
            let genoLine = V.fromList [HomRef, HomRef, HomAlt, HomAlt, HomAlt]
            computeAlleleCount genoLine ploidyVec indivNames [0, 1, 2, 3, 4] `shouldBe` (5, 7)
            let ploidyVec2 = V.fromList [Haploid, Haploid, Haploid, Haploid, Haploid]
            computeAlleleCount genoLine ploidyVec2 indivNames [0, 1, 2, 3, 4] `shouldBe` (3, 5)
        it "should throw if Hets are encountered with Haploid ploidy" $ do
            let genoLine = V.fromList [Het, HomRef, HomAlt, HomAlt, HomAlt]
            let ploidyVec = V.fromList [Haploid, Haploid, Haploid, Diploid, Diploid]
            (print (computeAlleleCount genoLine ploidyVec indivNames [0, 1, 2, 3, 4])) `shouldThrow` isGenotypeException
  where
    isGenotypeException (PoseidonGenotypeException "Sample Ind0 is heterozygous, but should be haploid. Check if the Ploidy-information in the Janno-file is correct") = True
    isGenotypeException _ = False

