{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Poseidon.GenotypeData where

import           Poseidon.Utils             (PoseidonException (..))

import           Control.Monad              (forM, forM_, void, when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.:), (.:?), (.=))
import           Data.ByteString            (isPrefixOf)
import           Data.List                  (nub, sort)
import           Data.Maybe                 (catMaybes, isNothing)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Pipes                      (Producer, (>->), Pipe (..),
                                            await, yield)
import           Pipes.OrderedZip           (orderCheckPipe, orderedZipAll)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe, SafeT (..))
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine,
                                             readEigenstrat, readEigenstratInd)
import           SequenceFormats.Plink      (readFamFile, readPlink)
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.Directory           (doesFileExist)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (hFlush, hPutStr, hPutStrLn, stderr)
-- | A datatype to specify genotype files
data GenotypeDataSpec = GenotypeDataSpec
    { format         :: GenotypeFormatSpec
    -- ^ the genotype format
    , genoFile       :: FilePath
    -- ^ path to the geno file
    , genoFileChkSum :: Maybe String
    -- ^ the optional checksum for the geno file
    , snpFile        :: FilePath
    -- ^ path to the snp file
    , snpFileChkSum  :: Maybe String
    -- ^ the optional checksum for the Snp file
    , indFile        :: FilePath
    -- ^ path to the ind file
    , indFileChkSum  :: Maybe String
    -- ^ the optional checksum for the indfile
    , snpSet         :: SNPSetSpec
    -- ^ the SNP set de facto listed in the genotype data
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of GenotypeDataSpec from JSON files
instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> GenotypeDataSpec
        <$> v .: "format"
        <*> v .: "genoFile"
        <*> v .:? "genoFileChkSum"
        <*> v .: "snpFile"
        <*> v .:? "snpFileChkSum"
        <*> v .: "indFile"
        <*> v .:? "indFileChkSum"
        <*> v .: "snpSet"

instance ToJSON GenotypeDataSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "format"            .= format x,
        "genoFile"          .= genoFile x,
        "genoFileChkSum"    .= genoFileChkSum x,
        "snpFile"           .= snpFile x,
        "snpFileChkSum"     .= snpFileChkSum x,
        "indFile"           .= indFile x,
        "indFileChkSum"     .= indFileChkSum x,
        "snpSet"            .= snpSet x
        ]

-- | A data type representing the options fo the genotype format
data GenotypeFormatSpec = GenotypeFormatEigenstrat
    | GenotypeFormatPlink
    deriving (Eq)

instance Show GenotypeFormatSpec where
    show GenotypeFormatPlink      = "PLINK"
    show GenotypeFormatEigenstrat = "EIGENSTRAT"

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

data SNPSetSpec = 
        SNPSet1240K
    |   SNPSetHumanOrigins
    |   SNPSetOther
    deriving (Eq)

instance Show SNPSetSpec where
    show SNPSet1240K        = "1240k"
    show SNPSetHumanOrigins = "HumanOrigins"
    show SNPSetOther        = "Other"

instance FromJSON SNPSetSpec where
    parseJSON = withText "snpSet" $ \v -> case v of
        "1240k"         -> pure SNPSet1240K
        "HumanOrigins"  -> pure SNPSetHumanOrigins
        "Other"         -> pure SNPSetOther
        _               -> fail ("unknown snpSet " ++ T.unpack v)

instance ToJSON SNPSetSpec where
    toJSON a = case a of
        SNPSet1240K         -> "1240k"
        SNPSetHumanOrigins  -> "HumanOrigins"
        SNPSetOther         -> "Other"

snpSetMergeList :: [SNPSetSpec] -> Bool -> SNPSetSpec
snpSetMergeList (x:xs) intersect = foldr (\a b -> snpSetMerge a b intersect) x xs

snpSetMerge :: SNPSetSpec -> SNPSetSpec -> Bool -> SNPSetSpec
snpSetMerge SNPSet1240K         SNPSet1240K         _       = SNPSet1240K
snpSetMerge SNPSetHumanOrigins  SNPSetHumanOrigins  _       = SNPSetHumanOrigins
snpSetMerge SNPSetOther         _                   _       = SNPSetOther
snpSetMerge _                   SNPSetOther         _       = SNPSetOther
snpSetMerge SNPSet1240K         SNPSetHumanOrigins  True    = SNPSetHumanOrigins
snpSetMerge SNPSetHumanOrigins  SNPSet1240K         True    = SNPSetHumanOrigins
snpSetMerge SNPSet1240K         SNPSetHumanOrigins  False   = SNPSet1240K
snpSetMerge SNPSetHumanOrigins  SNPSet1240K         False   = SNPSet1240K

-- | A function to return a list of all individuals in the genotype files of a package.
loadIndividuals :: FilePath -- ^ the base directory
               -> GenotypeDataSpec -- ^ the Genotype spec
               -> IO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
loadIndividuals d gd =
    case format gd of
        GenotypeFormatEigenstrat -> readEigenstratInd (d </> indFile gd)
        GenotypeFormatPlink      -> readFamFile (d </> indFile gd)

-- | A function to read the genotype data of a package
loadGenotypeData :: (MonadSafe m) =>
                   FilePath -- ^ the base path
                -> GenotypeDataSpec -- ^ the genotype spec
                -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line.
loadGenotypeData baseDir (GenotypeDataSpec format_ genoF _ snpF _ indF _ snpSet) =
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstrat (baseDir </> genoF) (baseDir </> snpF) (baseDir </> indF)
        GenotypeFormatPlink      -> readPlink (baseDir </> genoF) (baseDir </> snpF) (baseDir </> indF)

-- | A function to read genotype data jointly from multiple packages
loadJointGenotypeData :: (MonadSafe m) => Bool -- ^ whether to show all warnings
                     -> Bool -- ^ whether to generate an intersection instead of a union of all input files
                     -> [(FilePath, GenotypeDataSpec)]-- ^ A list of tuples of base directories and genotype data
                     -> m ([EigenstratIndEntry], Producer (EigenstratSnpEntry, GenoLine) m ())
                     -- ^ a pair of the EigenstratIndEntries and a Producer over the Snp position values and the genotype line, joined across all packages.
loadJointGenotypeData showAllWarnings intersect gdTuples = do
    genotypeTuples <- sequence [loadGenotypeData baseDir gd | (baseDir, gd) <- gdTuples]
    let indEntries      = map fst genotypeTuples
        jointIndEntries = concat indEntries
        nrInds          = map length indEntries
        jointProducer   = (orderedZipAll compFunc . map snd) genotypeTuples >->
            P.filter filterUnionOrIntersection >-> P.mapM (joinEntries showAllWarnings nrInds)
    return (jointIndEntries, void jointProducer)
  where
    compFunc :: (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine) -> Ordering
    compFunc (EigenstratSnpEntry c1 p1 _ _ _ _, _) (EigenstratSnpEntry c2 p2 _ _ _ _, _) = compare (c1, p1) (c2, p2)
    filterUnionOrIntersection :: [Maybe (EigenstratSnpEntry, GenoLine)] -> Bool
    filterUnionOrIntersection maybeTuples = not intersect || not (any isNothing maybeTuples)

joinEntries :: (MonadSafe m) => Bool -> [Int] -> [Maybe (EigenstratSnpEntry, GenoLine)] -> m (EigenstratSnpEntry, GenoLine)
joinEntries showAllWarnings nrInds maybeTupleList = do
    let allSnpEntries    = map fst . catMaybes $ maybeTupleList
    consensusSnpEntry <- getConsensusSnpEntry showAllWarnings allSnpEntries
    recodedGenotypes <- forM (zip nrInds maybeTupleList) $ \(n, maybeTuple) ->
        case maybeTuple of
            Nothing -> return (V.replicate n Missing)
            Just (snpEntry, genoLine) ->
                genotypes2alleles snpEntry genoLine >>= recodeAlleles consensusSnpEntry
    return (consensusSnpEntry, V.concat recodedGenotypes)

getConsensusSnpEntry :: (MonadSafe m) => Bool -> [EigenstratSnpEntry] -> m EigenstratSnpEntry
getConsensusSnpEntry showAllWarnings snpEntries = do
    let chrom = snpChrom . head $ snpEntries
        pos = snpPos . head $ snpEntries
        uniqueIds = nub . map snpId $ snpEntries
        uniqueGenPos = sort . nub . map snpGeneticPos $ snpEntries
        allAlleles    = concat $ [[r, a] | EigenstratSnpEntry _ _ _ _ r a <- snpEntries]
        uniqueAlleles = nub . filter (\a -> a /= 'N' && a /= '0' && a /= 'X') $ allAlleles
    id_ <- case uniqueIds of
        [i] -> return i
        _ -> do
            -- multiple Ids: Picking the first rs-number if possible, otherwise the first one.
            let rsIds = filter (isPrefixOf "rs") uniqueIds
                selectedId = case rsIds of
                    (i:_) -> i
                    _     -> head uniqueIds
            when showAllWarnings $
                liftIO . hPutStrLn stderr $ "Warning: Found inconsistent SNP IDs: " ++ show uniqueIds ++
                    ". Choosing " ++ show selectedId
            return selectedId
    genPos <- case uniqueGenPos of
        [p] -> return p
        [0.0, p] -> return p -- 0.0 is considered "no data" in genetic position column
        _ -> do
            -- multiple non-zero genetic positions. Choosing the largest one.
            let selectedGenPos = maximum uniqueGenPos
            when showAllWarnings $
                liftIO . hPutStrLn stderr $ "Warning: Found inconsistent genetic positions in SNP " ++ show id_ ++
                    ": " ++ show uniqueGenPos ++ ". Choosing " ++ show selectedGenPos
            return selectedGenPos
    case uniqueAlleles of
        [] -> do
            -- no non-missing alleles found
            when showAllWarnings $
                liftIO . hPutStrLn stderr $ "Warning: SNP " ++ show id_ ++ " appears to have no data (both ref and alt allele are blank"
            return (EigenstratSnpEntry chrom pos genPos id_ 'N' 'N')
        [r] -> do
            -- only one non-missing allele found
            when showAllWarnings $
                liftIO . hPutStrLn stderr $ "Warning: SNP " ++ show id_ ++ " appears to be monomorphic (only one of ref and alt alleles are non-blank)"
            return (EigenstratSnpEntry chrom pos genPos id_ 'N' r)
        [ref, alt] -> return (EigenstratSnpEntry chrom pos genPos id_ ref alt)
        _ -> throwM $ PoseidonGenotypeException ("Incongruent alleles: " ++ show snpEntries)

genotypes2alleles :: (MonadThrow m) => EigenstratSnpEntry -> GenoLine -> m (V.Vector (Char, Char))
genotypes2alleles snpEntry@(EigenstratSnpEntry chrom pos _ _ ref alt) = V.mapM g2a
  where
    g2a :: (MonadThrow m) => GenoEntry -> m (Char, Char)
    g2a HomRef =
        if ref `notElem` na
        then return (ref, ref)
        else throwM (PoseidonGenotypeException (show snpEntry ++ ": encountered illegal genotype Hom-Ref with Ref-Allele missing"))
    g2a Het =
        if ref `notElem` na && alt `notElem` na
        then return (ref, alt)
        else throwM (PoseidonGenotypeException (show snpEntry ++ ": encountered illegal genotype Het with Ref-Allele or Alt-Allele missing"))
    g2a HomAlt =
        if alt `notElem` na
        then return (alt, alt)
        else throwM (PoseidonGenotypeException (show snpEntry ++ ": encountered illegal genotype Hom-Alt with Alt-Allele missing"))
    g2a Missing = return ('N', 'N')
    na = ['0', 'N', 'X']

recodeAlleles :: (MonadThrow m) => EigenstratSnpEntry -> V.Vector (Char, Char) -> m GenoLine
recodeAlleles snpEntry@(EigenstratSnpEntry _ _ _ _ ref alt) = V.mapM a2g
  where
    a2g :: (MonadThrow m) => (Char, Char) -> m GenoEntry
    a2g (a1, a2)
        | (a1, a2)  == (ref, ref)                            && ref `notElem` na                       = return HomRef
        | (a1, a2)  == (alt, alt)                            && alt `notElem` na                       = return HomAlt
        | ((a1, a2) == (ref, alt) || (a1, a2) == (alt, ref)) && (ref `notElem` na && alt `notElem` na) = return Het
        | a1 `elem` na && a2 `elem` na                                                                 = return Missing
        | otherwise                                                                                    = throwM (err a1 a2)
    err a1 a2 = PoseidonGenotypeException ("At snp " ++ show snpEntry ++ ": cannot recode allele-pair " ++ show (a1, a2) ++ " with ref,alt alleles " ++ show (ref, alt))
    na = ['0', 'N', 'X']

printSNPCopyProgress :: Pipe a a (SafeT IO) ()
printSNPCopyProgress = loop 0
  where
    loop n = do
        when (n `rem` 1000 == 0) $ do
            liftIO $ hClearLine stderr
            liftIO $ hSetCursorColumn stderr 0
            liftIO $ hPutStr stderr ("> " ++ show n ++ " ")
            liftIO $ hFlush stderr
        x <- await
        yield x
        loop (n+1)
