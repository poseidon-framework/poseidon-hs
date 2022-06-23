{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Poseidon.GenotypeData where

import           Poseidon.Utils             (PoseidonException (..))

import           Control.Exception          (throwIO)
import           Control.Monad              (forM, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.:), (.:?), (.=))
import           Data.ByteString            (isPrefixOf)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import           Data.List                  (nub, sort)
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Pipes                      (Pipe, Producer)
import qualified Pipes.Prelude as P
import           Pipes.Safe                 (MonadSafe)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine,
                                             readEigenstrat, readEigenstratInd)
import           SequenceFormats.Plink      (readFamFile, readPlink)
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.FilePath            ((</>))
import           System.IO                  (hFlush, hPutStr, stderr)

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
    , snpSet         :: Maybe SNPSetSpec
    -- ^ the SNP set de facto listed in the genotype data
    }
    deriving (Show, Eq)

-- | To facilitate automatic parsing of GenotypeDataSpec from JSON files
instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> GenotypeDataSpec
        <$> v .:  "format"
        <*> v .:  "genoFile"
        <*> v .:? "genoFileChkSum"
        <*> v .:  "snpFile"
        <*> v .:? "snpFileChkSum"
        <*> v .:  "indFile"
        <*> v .:? "indFileChkSum"
        <*> v .:? "snpSet"

instance ToJSON GenotypeDataSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
        "format"        .= format x,
        "genoFile"      .= genoFile x,
        "genoFileChkSum".= genoFileChkSum x,
        "snpFile"       .= snpFile x,
        "snpFileChkSum" .= snpFileChkSum x,
        "indFile"       .= indFile x,
        "indFileChkSum" .= indFileChkSum x,
        "snpSet"        .= snpSet x
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

data SNPSetSpec = SNPSet1240K
    | SNPSetHumanOrigins
    | SNPSetOther
    deriving (Eq)

instance Show SNPSetSpec where
    show SNPSet1240K        = "1240K"
    show SNPSetHumanOrigins = "HumanOrigins"
    show SNPSetOther        = "Other"

instance FromJSON SNPSetSpec where
    parseJSON = withText "snpSet" $ \v -> case v of
        "1240K"        -> pure SNPSet1240K
        "HumanOrigins" -> pure SNPSetHumanOrigins
        "Other"        -> pure SNPSetOther
        _              -> fail ("unknown snpSet " ++ T.unpack v)

instance ToJSON SNPSetSpec where
    toJSON a = case a of
        SNPSet1240K        -> "1240K"
        SNPSetHumanOrigins -> "HumanOrigins"
        SNPSetOther        -> "Other"

snpSetMergeList :: [SNPSetSpec] -> Bool -> SNPSetSpec
snpSetMergeList (x:xs) intersect = foldr (\a b -> snpSetMerge a b intersect) x xs
snpSetMergeList _ _ = error "snpSetMergeList: This should never happen"

snpSetMerge :: SNPSetSpec -> SNPSetSpec -> Bool -> SNPSetSpec
snpSetMerge SNPSet1240K         SNPSet1240K         _     = SNPSet1240K
snpSetMerge SNPSetHumanOrigins  SNPSetHumanOrigins  _     = SNPSetHumanOrigins
snpSetMerge SNPSetOther         _                   _     = SNPSetOther
snpSetMerge _                   SNPSetOther         _     = SNPSetOther
snpSetMerge SNPSet1240K         SNPSetHumanOrigins  True  = SNPSetHumanOrigins
snpSetMerge SNPSetHumanOrigins  SNPSet1240K         True  = SNPSetHumanOrigins
snpSetMerge SNPSet1240K         SNPSetHumanOrigins  False = SNPSet1240K
snpSetMerge SNPSetHumanOrigins  SNPSet1240K         False = SNPSet1240K

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
loadGenotypeData baseDir (GenotypeDataSpec format_ genoF _ snpF _ indF _ _) =
    case format_ of
        GenotypeFormatEigenstrat -> readEigenstrat (baseDir </> genoF) (baseDir </> snpF) (baseDir </> indF)
        GenotypeFormatPlink      -> readPlink (baseDir </> genoF) (baseDir </> snpF) (baseDir </> indF)

joinEntries :: (MonadIO m) => [Int] -> [String] -> [Maybe (EigenstratSnpEntry, GenoLine)] -> m ([String], EigenstratSnpEntry, GenoLine)
joinEntries nrInds pacNames maybeTupleList = do
    let allSnpEntries = map fst . catMaybes $ maybeTupleList
    (consensusSnpEntryWarnings, consensusSnpEntry) <- getConsensusSnpEntry allSnpEntries
    recodedGenotypes <- forM (zip3 nrInds pacNames maybeTupleList) $ \(n, name, maybeTuple) ->
        case maybeTuple of
            Nothing -> return (V.replicate n Missing)
            Just (snpEntry, genoLine) -> case recodeAlleles consensusSnpEntry snpEntry genoLine of
                Left err -> do
                    let msg = "Error in genotype data of package " ++ name ++ ": " ++ err
                    liftIO . throwIO $ PoseidonGenotypeException msg
                Right x -> return x
    return (consensusSnpEntryWarnings, consensusSnpEntry, V.concat recodedGenotypes)

getConsensusSnpEntry :: (MonadIO m) => [EigenstratSnpEntry] -> m ([String], EigenstratSnpEntry)
getConsensusSnpEntry snpEntries = do
    let chrom = snpChrom . head $ snpEntries
        pos = snpPos . head $ snpEntries
        uniqueIds = nub . map snpId $ snpEntries
        uniqueGenPos = sort . nub . map snpGeneticPos $ snpEntries
        allAlleles    = concat $ [[r, a] | EigenstratSnpEntry _ _ _ _ r a <- snpEntries]
        uniqueAlleles = nub . filter (\a -> a /= 'N' && a /= '0' && a /= 'X') $ allAlleles
    id_ <- case uniqueIds of
        [i] -> return (Nothing, i)
        _ -> do -- multiple Ids: Picking the first rs-number if possible, otherwise the first one.
            let rsIds = filter (isPrefixOf "rs") uniqueIds
                selectedId = case rsIds of
                    (i:_) -> i
                    _     -> head uniqueIds
            return (
                Just $ "Found inconsistent SNP IDs: " ++ show uniqueIds ++ ". Choosing " ++ show selectedId,
                selectedId
                )
    genPos <- case uniqueGenPos of
        [p] -> return (Nothing, p)
        [0.0, p] -> return (Nothing, p) -- 0.0 is considered "no data" in genetic position column
        _ -> do -- multiple non-zero genetic positions. Choosing the largest one.
            let selectedGenPos = maximum uniqueGenPos
            return (
                Just $ "Found inconsistent genetic positions in SNP " ++ show (snd id_) ++ ": " ++ show uniqueGenPos ++ ". Choosing " ++ show selectedGenPos,
                selectedGenPos
                )
    case uniqueAlleles of
        [] -> do -- no non-missing alleles found
            return (
                catMaybes [fst id_, fst genPos, Just $ "SNP " ++ show (snd id_) ++ " appears to have no data (both ref and alt allele are blank"],
                EigenstratSnpEntry chrom pos (snd genPos) (snd id_) 'N' 'N'
                )
        [r] -> do -- only one non-missing allele found
            return (
                catMaybes [fst id_, fst genPos, Just $ "SNP " ++ show id_ ++ " appears to be monomorphic (only one of ref and alt alleles are non-blank)"],
                EigenstratSnpEntry chrom pos (snd genPos) (snd id_) 'N' r
                )
        [ref, alt] -> 
            return (
                catMaybes [fst id_, fst genPos],
                EigenstratSnpEntry chrom pos (snd genPos) (snd id_) ref alt
            )
        _ -> liftIO . throwIO $ PoseidonGenotypeException ("Incongruent alleles: " ++ show snpEntries)

recodeAlleles :: EigenstratSnpEntry -> EigenstratSnpEntry -> GenoLine -> Either String GenoLine
recodeAlleles consensusSnpEntry snpEntry genoLine = do
    let (EigenstratSnpEntry _ _ _ _ consRefA consAltA) = consensusSnpEntry
    let (EigenstratSnpEntry _ _ _ _ refA altA) = snpEntry
    let maybeRecodedGenoline = case (isMissing consRefA, isMissing consAltA) of
            (False, False) -> maybeFlipGenoLine1 consRefA consAltA refA altA
            (False, True)  -> maybeFlipGenoLine2 consRefA          refA altA
            (True, False)  -> maybeFlipGenoLine3          consAltA refA altA
            (True, True)   -> maybeFlipGenoLine4
    case maybeRecodedGenoline of
        Left err -> Left ("At snp " ++ show snpEntry ++ ": allele coding error due to inconsistent \
                           \alleles with consensus alleles ref = " ++ [consRefA] ++ ", alt = " ++ [consAltA] ++
                           ". Error: " ++ err)
        Right recodedGenoLine -> return recodedGenoLine
  where
    isMissing '0' = True
    isMissing 'N' = True
    isMissing _   = False
    maybeFlipGenoLine1 consRefA consAltA refA altA
        | (refA, altA) == (consRefA, consAltA) = return genoLine -- simple concordance
        | (refA, altA) == (consAltA, consRefA) = return (V.map flipGeno genoLine) -- alleles flipped
        | refA == consRefA                     = checked HomRef $ return genoLine -- refs equal, alts different, need everything HomRef or Missing
        | altA == consAltA                     = checked HomAlt $ return genoLine -- alts equal, refs different, need everything HomAlt
        | refA == consAltA                     = checked HomRef $ return (V.map flipGeno genoLine) -- need everything HomRef, then flip
        | altA == consRefA                     = checked HomAlt $ return (V.map flipGeno genoLine) -- need everything HomAlt, then flip
        | otherwise                            = checked Missing $ return genoLine
    maybeFlipGenoLine2 consRefA refA altA
        | refA == consRefA                     = checked HomRef $ return genoLine -- refs equal, need everything HomRef or Missing
        | altA == consRefA                     = checked HomAlt $ return (V.map flipGeno genoLine) -- ref flipped, need everything HomAlt or Missing, then flip
        | otherwise                            = checked Missing $ return genoLine
    maybeFlipGenoLine3 consAltA refA altA
        | refA == consAltA                     = checked HomRef $ return (V.map flipGeno genoLine) -- alt flipped, need everything HomAlt or Missing, then flip
        | altA == consAltA                     = checked HomAlt $ return genoLine -- alts equal, need everything HomAlt or Missing
        | otherwise                            = checked Missing $ return genoLine
    maybeFlipGenoLine4 = checked Missing $ return genoLine
    checked Missing action = if V.any (/= Missing) genoLine then Left "Requiring all genotype missing" else action
    checked t       action = if V.any (\g -> g /= Missing && g /= t) genoLine then Left ("requiring all genotypes missing or " ++ show t) else action
    flipGeno HomRef = HomAlt
    flipGeno HomAlt = HomRef
    flipGeno g      = g

printSNPCopyProgress :: (MonadIO m) => Pipe a a m ()
printSNPCopyProgress = do
    counterRef <- liftIO $ newIORef 0
    P.mapM (withCounter counterRef)
  where
    withCounter :: (MonadIO m) => IORef Int -> a -> m a
    withCounter counterRef val = do
        n <- liftIO $ readIORef counterRef
        when (n `rem` 1000 == 0) $ do
            liftIO $ hClearLine stderr
            liftIO $ hSetCursorColumn stderr 0
            liftIO $ hPutStr stderr ("> " ++ show n ++ " ")
            liftIO $ hFlush stderr
        liftIO $ modifyIORef counterRef (+1)
        return val

selectIndices :: [Int] -> (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine)
selectIndices indices (snpEntry, genoLine) = (snpEntry, V.fromList [genoLine V.! i | i <- indices])
