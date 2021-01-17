{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Poseidon.GenotypeData where

import           Poseidon.Utils             (PoseidonException (..))

import           Control.Monad              (forM, forM_, when, void)
import           Control.Monad.Catch        (throwM, MonadThrow)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.:), (.=))
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Pipes                      (Producer, (>->))
import           Pipes.OrderedZip           (orderCheckPipe, orderedZip)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine,
                                             readEigenstrat, readEigenstratInd)
import           SequenceFormats.Plink      (readFamFile, readPlink)
import           System.Directory           (doesFileExist)

-- | A datatype to specify genotype files
data GenotypeDataSpec = GenotypeDataSpec
    { format   :: GenotypeFormatSpec
    -- ^ the genotype format
    , genoFile :: FilePath
    -- ^ path to the geno file
    , snpFile  :: FilePath
    -- ^ path to the snp file
    , indFile  :: FilePath
    -- ^ path to the ind file
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of GenotypeDataSpec from JSON files
instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> GenotypeDataSpec
        <$> v .: "format"
        <*> v .: "genoFile"
        <*> v .: "snpFile"
        <*> v .: "indFile"

instance ToJSON GenotypeDataSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "format" .= format x,
        "genoFile" .= genoFile x,
        "snpFile" .= snpFile x,
        "indFile" .= indFile x
        ]

-- | A data type representing the options fo the genotype format
data GenotypeFormatSpec = GenotypeFormatEigenstrat
    | GenotypeFormatPlink
    deriving (Show, Eq)

-- | To facilitate automatic parsing of GenotypeFormatSpec from JSON files
instance FromJSON GenotypeFormatSpec where
    parseJSON = withText "format" $ \v -> case v of
        "EIGENSTRAT" -> pure GenotypeFormatEigenstrat
        "PLINK"      -> pure GenotypeFormatPlink
        _            -> fail ("unknown format " ++ T.unpack v)

instance ToJSON GenotypeFormatSpec where
    toJSON a = case a of
        GenotypeFormatPlink      -> "PLINK"
        GenotypeFormatEigenstrat -> "EIGENSTRAT"

zipAll :: MonadSafe m => [Int] -> [Producer (EigenstratSnpEntry, GenoLine) m r] -> Producer [(EigenstratSnpEntry, GenoLine)] m [r]
zipAll _                   []            = error "zipAll - should never happen (1)"
zipAll []                  _             = error "zipAll - should never happen (2)"
zipAll _                   [prod]        = fmap (\x -> [x]) (prod >-> orderCheckPipe compFunc1) >-> P.map (\x ->[x])
zipAll (nrHaps:restNrHaps) (prod1:prods) =
    fmap (\(r, rs) -> (r:rs)) (orderedZip compFunc2 (prod1 >-> orderCheckPipe compFunc1) (zipAll restNrHaps prods)) >-> P.map processMaybeTuples
  where
    processMaybeTuples :: (Maybe (EigenstratSnpEntry, GenoLine), Maybe [(EigenstratSnpEntry, GenoLine)]) -> [(EigenstratSnpEntry, GenoLine)]
    processMaybeTuples (Nothing,        Nothing)          = error "processMaybeTuples: should never happen"
    processMaybeTuples (Just (es, gl),  Nothing)          = (es, gl) : [(es, V.replicate l Missing) | l <- restNrHaps]
    processMaybeTuples (Nothing,        Just restEntries) = (fst (head restEntries), V.replicate nrHaps Missing) : restEntries
    processMaybeTuples (Just (es, gl1), Just restEntries) = (es, gl1) : restEntries

compFunc1 :: (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine) -> Ordering
compFunc1 (EigenstratSnpEntry c1 p1 _ _ _ _, _) (EigenstratSnpEntry c2 p2 _ _ _ _, _) = compare (c1, p1) (c2, p2)

compFunc2 :: (EigenstratSnpEntry, GenoLine) -> [(EigenstratSnpEntry, GenoLine)] -> Ordering
compFunc2 (EigenstratSnpEntry c1 p1 _ _ _ _, _) ((EigenstratSnpEntry c2 p2 _ _ _ _, _):_) = compare (c1, p1) (c2, p2)
compFunc2 _                                     []                                        = error "compFunc2 - should never happen"

-- | A function to return a list of all individuals in the genotype files of a package.
loadIndividuals :: GenotypeDataSpec -- ^ the Genotype spec
               -> IO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
loadIndividuals (GenotypeDataSpec format_ _ _ indF) =
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstratInd indF
        GenotypeFormatPlink      -> readFamFile indF

-- | A function to read the genotype data of a package
loadGenotypeData :: (MonadSafe m) => GenotypeDataSpec -- ^ the genotype spec
                -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line.
loadGenotypeData (GenotypeDataSpec format_ genoF snpF indF) = do
    forM_ [genoF, snpF, indF] $ (\f -> do
        fileE <- liftIO (doesFileExist f)
        when (not fileE) $ throwM (PoseidonFileExistenceException ("File " ++ f ++ " does not exist")))
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstrat genoF snpF indF
        GenotypeFormatPlink      -> readPlink genoF snpF indF

-- | A function to read genotype data jointly from multiple packages
loadJointGenotypeData :: (MonadSafe m) => [GenotypeDataSpec] -- ^ A list of genotype specifications
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
loadJointGenotypeData gds = do
    genotypeTuples <- mapM loadGenotypeData gds
    let indEntries      = map fst genotypeTuples
        jointIndEntries = concat indEntries
        nrInds          = map length indEntries
        jointProducer   = (zipAll nrInds . map snd) genotypeTuples >-> P.mapM joinEntries
    return (jointIndEntries, void jointProducer)

joinEntries :: (MonadThrow m) => [(EigenstratSnpEntry, GenoLine)] -> m (EigenstratSnpEntry, GenoLine)
joinEntries tupleList = do
    let allSnpEntries    = map fst tupleList
        allGenoEntries   = map snd tupleList
    snpEntry@(EigenstratSnpEntry _ _ _ _ refA, altA) <- getConsensusSnpEntry allSnpEntries
    normalisedGenotypes <- forM (zip allSnpEntries allGenoEntries) $ \(es@(EigenstratSnpEntry _ _ _ _ refA1 altA1), genoLine) -> do
        when (not $ genotypesAreValid refA1 altA1 genoLine) $
            throwM $ PoseidonGenotypeException ("encountered illegal genotypes at site " ++ show es ++ " with genotypes " ++ show genoLine)
        if alleleConcordant refA refA1 && alleleConcordant altA altA1
            then return genoLine
            else if alleleConcordant refA altA1 && alleleConcordant altA refA1
                    then return (flipGenotypes genoLine)
                    else error "This should not happen. Alleles should be congruent after running getConsensusSnpPair"
    return (snpEntry, V.concat normalisedGenotypes)

getConsensusSnpEntry :: (MonadThrow m) => [EigenstratSnpEntry] -> m EigenstratSnpEntry 
getConsensusSnpEntry snpEntries = do
    let chrom = snpChrom . head $ snpEntries
        pos = snpPos . head $ snpEntries
        uniqueIds = nub . map snpId $ snpEntries
        uniqueGenPos = sort . nub . map snpGeneticPos $ snpEntries
        allAlleles    = concat $ [[r, a] | EigenstratSnpEntry _ _ _ _ r a <- snpEntries]
        uniqueAlleles = nub . filter (\a -> a /= 'N' && a /= '0') $ allAlleles
    id_ <- case uniqueIds of
        [i] -> return i
        _ -> throwM $ PoseidonGenotypeException ("SNP IDs incongruent: " ++ show snpEntries)
    genPos <- case uniqueGenPos of
        [p] -> return p
        [0.0, p] -> return p
        _ -> throwM $ PoseidonGenotypeException ("SNP genetic positions incongruent: " ++ show snpEntries)
    [ref, alt] <- case uniqueAlleles of
        [] -> throwM $ PoseidonGenotypeException ("Illegal SNP, has only missing data: " ++ show snpEntries)
        [r] -> throwM $ PoseidonGenotypeException ("Illegal SNP, monomorphic: " ++ show snpEntries)
        [r, a] -> return [r, a]
        _ -> throwM $ PoseidonGenotypeException ("Incongruent alleles: " ++ show snpEntries)
    return (EigenstratSnpEntry chrom pos genoPos id_ ref alt)

genotypesAreValid :: (MonadThrow m) => Char -> Char -> GenoLine -> Bool
genotypesAreValid ref alt genoLine
    | ref `elem` na && alt `notElem` na = V.all (\c `notElem` [HomRef, Het]) genoLine
    | ref `notElem` na && alt `elem` na = V.all (\c `notElem` [HomAlt, Het]) genoLine
    | otherwise                         = True
  where
    na = ['0', 'N']

alleleConcordant :: Char -> Char -> Bool
alleleConcordant '0' _   = True
alleleConcordant _   '0' = True
alleleConcordant 'N' _   = True
alleleConcordant _   'N' = True
alleleConcordant a1  a2  = a1 == a2

flipGenotypes :: GenoLine -> GenoLine
flipGenotypes = V.map (\case
    HomRef  -> HomAlt
    Het     -> Het
    HomAlt  -> HomRef
    Missing -> Missing)
