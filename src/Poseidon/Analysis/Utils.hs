{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Analysis.Utils where

import           Control.Exception          (Exception, throw)
import           Control.Monad              (forM)
import           Data.Aeson                 ((.:))
import           Data.Aeson.Key             (toString)
import           Data.Aeson.KeyMap          (toList)
import           Data.Aeson.Types           (Object, Parser)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Pipes                      (Pipe, cat)
import qualified Pipes.Prelude              as P
import           Poseidon.ColumnTypesJanno  (GroupName (..),
                                             JannoGenotypePloidy (..))
import           Poseidon.ColumnTypesUtils  (ListColumn (..))
import           Poseidon.EntityTypes       (IndividualInfo (..),
                                             SignedEntitiesList,
                                             indInfoConformsToEntitySpecs,
                                             isLatestInCollection,
                                             makePacNameAndVersion)
import           Poseidon.Janno             (JannoRow (..), JannoRows (..))
import           Poseidon.Package           (PoseidonPackage (..),
                                             getJannoRowsFromPac)
import           Poseidon.Utils             (PoseidonException (..), PoseidonIO,
                                             logWarning)
import           SequenceFormats.Eigenstrat (EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine)
import           SequenceFormats.Utils      (Chrom)

-- | A datatype representing the two options for how to run the Block-Jackknife
data JackknifeMode = JackknifePerN Int
    | JackknifePerChromosome
    deriving (Show)

-- | A helper type to represent a genomic position.
type GenomPos = (Chrom, Int)

type GroupDef = (String, SignedEntitiesList)

type PloidyVec = V.Vector JannoGenotypePloidy

addGroupDefs :: [GroupDef] -> [PoseidonPackage] -> [PoseidonPackage]
addGroupDefs groupDefs pacs = do -- this loops through all input packages
    pac <- pacs
    isLatest <- isLatestInCollection pacs pac
    let newJanno = JannoRows $ do -- this loops through the janno-file
            jannoRow <- getJannoRowsFromPac pac
            let oldGroupNames = getListColumn . jGroupName $ jannoRow
            let additionalGroupNames = do -- this loops through each new group definition and returns those group names that apply to this janno-row
                    (groupName, signedEntityList) <- groupDefs
                    let indInfo = IndividualInfo (jPoseidonID jannoRow) (map (\(GroupName n) -> T.unpack n) oldGroupNames) (makePacNameAndVersion pac)
                    True <- return $ indInfoConformsToEntitySpecs indInfo isLatest signedEntityList -- this checks whether a new group-def applies to this janno-row
                    return . GroupName . T.pack $ groupName -- only returns if the previous row pattern-matched, i.e. if the group applies
            return $ jannoRow {jGroupName = ListColumn (oldGroupNames ++ additionalGroupNames)} -- returns a new janno-row with the new group definitions
    return $ pac {posPacJanno = newJanno} -- returns a new package with the new janno

parseGroupDefsFromJSON :: Object -> Parser [GroupDef]
parseGroupDefsFromJSON obj = forM (toList obj) $ \(key, _) -> do
    entities <- obj .: key
    return (toString key, entities)

computeAlleleCount :: GenoLine -> PloidyVec -> [String] -> [Int] -> (Int, Int)
computeAlleleCount line ploidyVec indivNames indices =
    let nrNonMissing = sum $ do
            i <- indices
            True <- return $ line V.! i /= Missing
            case ploidyVec V.! i of
                Haploid -> return 1
                Diploid -> return 2
        nrDerived = sum $ do
            i <- indices
            case line V.! i of
                HomRef  -> return (0 :: Int)
                Het     -> case ploidyVec V.! i of
                    Haploid -> throw . PoseidonGenotypeException $
                        "Sample " ++ indivNames !! i ++ " is heterozygous, but should be haploid. Check if the Ploidy-information in the Janno-file is correct"
                    Diploid -> return 1
                HomAlt  -> case ploidyVec V.! i of
                    Haploid -> return 1
                    Diploid -> return 2
                Missing -> return 0
    in  (nrDerived, nrNonMissing)

computeAlleleFreq :: GenoLine -> PloidyVec -> [String] -> [Int] -> Maybe Double
computeAlleleFreq line ploidyVec indivNames indices =
    let (nrDerivedHaps, nrNonMissingHaps) = computeAlleleCount line ploidyVec indivNames indices
    in  if nrNonMissingHaps > 0
        then Just (fromIntegral nrDerivedHaps / fromIntegral nrNonMissingHaps)
        else Nothing

-- ^ This is the original Delete-mj Jackknife based on Busing et al. 1999. The notation is the same.
-- theta_n is the full estimate. m_j is the weights of block j. theta_j is the
-- estimate based on the j'th block removed.
computeJackknifeOriginal :: Double -> [Double] -> [Double] -> (Double, Double)
computeJackknifeOriginal theta_n m_j_list theta_j_list =
    let g               = fromIntegral (length m_j_list)
        n               = sum m_j_list
        theta_Jackknife = g * theta_n - sum [(1.0 - m_j / n) * theta_j | (m_j, theta_j) <- zip m_j_list theta_j_list]
        h_j_list        = [n / m_j | m_j <- m_j_list]
        pseudo_values   = [h_j * theta_n  - (h_j - 1.0) * theta_j | (h_j, theta_j) <- zip h_j_list theta_j_list]
        sigma_square    = 1.0 / g * sum [1.0 / (h_j - 1.0) * (pseudo_value - theta_Jackknife) ^ (2 :: Int) | (h_j, pseudo_value) <- zip h_j_list pseudo_values]
    in  (theta_Jackknife, sqrt sigma_square)

-- ^ A simplified Jackknife formula based on an additive estimate, in which the full estimate equals the Jackknife estimate
computeJackknifeAdditive :: [Int] -> [Double] -> (Double, Double)
computeJackknifeAdditive weights values =
    let weights'    = map fromIntegral weights
        sumWeights  = sum weights'
        g           = fromIntegral (length weights)
        theta       = sum [mj * val | (mj, val) <- zip weights' values] / sumWeights
        sigmaSquare = sum [mj * (val - theta) ^ (2 :: Int) / (sumWeights - mj) | (mj, val) <- zip weights' values] / g
    in  (theta, sqrt sigmaSquare)

filterTransitions :: (Monad m) => Bool -> Pipe (EigenstratSnpEntry, a) (EigenstratSnpEntry, a) m r
filterTransitions noTransitions = if noTransitions then
        P.filter (\(EigenstratSnpEntry _ _ _ _ ref alt, _) -> isTransversion ref alt)
    else
        cat
    where
    isTransversion ref alt = not $ isTransition ref alt
    isTransition ref alt =
        ((ref == 'A') && (alt == 'G')) ||
        ((ref == 'G') && (alt == 'A')) ||
        ((ref == 'C') && (alt == 'T')) ||
        ((ref == 'T') && (alt == 'C'))

data XerxesException = PopConfigYamlException FilePath String
    | GroupDefException String
    | FStatException String
    deriving (Show)

instance Exception XerxesException

makePloidyVec :: JannoRows -> PoseidonIO PloidyVec
makePloidyVec (JannoRows jannoRows) = do
    ploidyList <- forM jannoRows $ \jannoRow -> do
        case jGenotypePloidy jannoRow of
            Nothing -> do
                logWarning $ "no ploidy information for " ++ jPoseidonID jannoRow ++
                    ". Assuming Diploid. Use the Janno-column \"Genotype_Ploidy\" to specify"
                return Diploid
            Just pl -> return pl
    return $ V.fromList ploidyList
