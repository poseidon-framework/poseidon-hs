{-# LANGUAGE OverloadedStrings #-}
module Poseidon.GenotypeData where

import           Poseidon.Utils             (PoseidonException (..))

import           Control.Monad              (forM, forM_, when)
import           Control.Monad.Catch        (throwM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.:), (.=))
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Yaml.Pretty.Extras    (ToPrettyYaml (..))
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

instance ToPrettyYaml GenotypeDataSpec where
    fieldOrder = const [
        "format",
        "genoFile",
        "snpFile",
        "indFile"
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
    return (jointIndEntries, jointProducer >> return ())
  where
    joinEntries :: (MonadSafe m) => [(EigenstratSnpEntry, GenoLine)] -> m (EigenstratSnpEntry, GenoLine)
    joinEntries tupleList = do
        let allSnpEntries                            = map fst tupleList
            allGenoEntries                           = map snd tupleList
            (EigenstratSnpEntry _ _ _ _ refA1 altA1) = head allSnpEntries
        allEntriesFlipped <- forM (zip (tail allSnpEntries) (tail allGenoEntries)) $ \(es@(EigenstratSnpEntry _ _ _ _ refA altA), genoLine) ->
            if alleleConcordant refA refA1 && alleleConcordant altA altA1
            then return (es, genoLine)
            else if alleleConcordant refA altA1 && alleleConcordant altA refA1
                    then return (es {snpRef = altA, snpAlt = refA}, flipGenotypes genoLine)
                    else throwM (PoseidonGenotypeException ("SNP alleles are incongruent " ++ show allSnpEntries))
        let allSnpEntriesFlipped  = (head allSnpEntries) : map fst allEntriesFlipped
            allGenoEntriesFlipped = (head allGenoEntries) : map snd allEntriesFlipped
        return (makeSnpEntriesConcordant allSnpEntriesFlipped, V.concat allGenoEntriesFlipped)
    alleleConcordant :: Char -> Char -> Bool
    alleleConcordant '0' _   = True
    alleleConcordant _   '0' = True
    alleleConcordant 'N' _   = True
    alleleConcordant _   'N' = True
    alleleConcordant a1  a2  = a1 == a2
    flipGenotypes :: GenoLine -> GenoLine
    flipGenotypes = V.map (\a -> case a of
        HomRef  -> HomAlt
        Het     -> Het
        HomAlt  -> HomRef
        Missing -> Missing)
    makeSnpEntriesConcordant :: [EigenstratSnpEntry] -> EigenstratSnpEntry
    makeSnpEntriesConcordant snpEntries@(e:_) =
        let allRefs            = map snpRef snpEntries
            allAlts            = map snpAlt snpEntries
            allInformativeRefs = filter (\c -> c /= '0' && c /= 'N') allRefs
            allInformativeAlts = filter (\c -> c /= '0' && c /= 'N') allAlts
            ref = if not (null allInformativeRefs) then head allInformativeRefs else head allRefs
            alt = if not (null allInformativeAlts) then head allInformativeAlts else head allAlts
        in  e {snpRef = ref, snpAlt = alt}
    makeSnpEntriesConcordant _ = error "should not happen"
