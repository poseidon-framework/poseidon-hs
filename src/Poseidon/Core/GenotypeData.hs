{-# LANGUAGE OverloadedStrings #-}
module Poseidon.Core.GenotypeData where

import           Paths_poseidon_hs                (version)
import           Poseidon.Core.ColumnTypesJanno   (GroupName (..),
                                                   JannoGenotypePloidy (..),
                                                   PoseidonID (..))
import           Poseidon.Core.ColumnTypesUtils   (ListColumn (..))
import           Poseidon.Core.Janno              (JannoRow (..))
import           Poseidon.Core.Utils              (LogA, PoseidonException (..),
                                                   PoseidonIO,
                                                   envInputPlinkMode, logInfo,
                                                   logWarning, logWithEnv,
                                                   padLeft)

import           Control.Monad                    (forM, forM_, unless, when)
import           Control.Monad.Catch              (MonadThrow, throwM)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Aeson                       (FromJSON, ToJSON, object,
                                                   parseJSON, toJSON,
                                                   withObject, withText, (.:),
                                                   (.:?), (.=))
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString                  (isPrefixOf)
import qualified Data.ByteString.Char8            as B
import           Data.IORef                       (modifyIORef, newIORef,
                                                   readIORef)
import           Data.List                        (intercalate, nub, sort)
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import           Data.Time                        (NominalDiffTime, UTCTime,
                                                   diffUTCTime, getCurrentTime)
import qualified Data.Vector                      as V
import           Data.Version                     (showVersion)
import           Pipes                            (Consumer, Pipe, Producer,
                                                   cat, for, yield, (>->))
import qualified Pipes.Prelude                    as P
import           Pipes.Safe                       (MonadSafe, runSafeT)
import           SequenceFormats.Eigenstrat       (EigenstratIndEntry (..),
                                                   EigenstratSnpEntry (..),
                                                   GenoEntry (..), GenoLine,
                                                   Sex (..), parseSex,
                                                   readEigenstrat,
                                                   readEigenstratInd)
import           SequenceFormats.FreqSum          (FreqSumEntry (..))
import           SequenceFormats.Plink            (plinkFam2EigenstratInd,
                                                   readFamFile, readPlink)
import           SequenceFormats.VCF              (VCFentry (..),
                                                   VCFheader (..),
                                                   readVCFfromFile,
                                                   vcfToFreqSumEntry,
                                                   writeVCFfile)
import           System.FilePath                  (takeDirectory, takeFileName,
                                                   (</>))

data GenoDataSource = PacBaseDir
    { getPacBaseDir :: FilePath
    }
    | GenoDirect
    { getGenoDirect :: GenotypeDataSpec
    }
    deriving Show

data GenotypeDataSpec = GenotypeDataSpec {
    genotypeFileSpec        :: GenotypeFileSpec,
    genotypeSnpSet          :: Maybe SNPSetSpec,
    genotypeRefAssemblyName :: Maybe String,
    genotypeRefAssemblyURL  :: Maybe String
} deriving (Show, Eq)

data GenotypeFileSpec = GenotypeEigenstrat {
    _esGenoFile       :: FilePath,
    _esGenoFileChkSum :: Maybe String,
    _esSnpFile        :: FilePath,
    _esSnpFileChkSum  :: Maybe String,
    _esIndFile        :: FilePath,
    _esIndFileChkSum  :: Maybe String
} | GenotypePlink {
    _plGenoFile       :: FilePath,
    _plGenoFileChkSum :: Maybe String,
    _plSnpFile        :: FilePath,
    _plSnpFileChkSum  :: Maybe String,
    _plIndFile        :: FilePath,
    _plIndFileChkSum  :: Maybe String
} | GenotypeVCF {
    _vcfGenoFile       :: FilePath,
    _vcfGenoFileChkSum :: Maybe String
} deriving (Show, Eq)

data GenotypeOutFormatSpec = GenotypeOutFormatPlink | GenotypeOutFormatEigenstrat | GenotypeOutFormatVCF

instance Show GenotypeOutFormatSpec where
    show GenotypeOutFormatEigenstrat = "EIGENSTRAT"
    show GenotypeOutFormatPlink      = "PLINK"
    show GenotypeOutFormatVCF        = "VCF"

instance Read GenotypeOutFormatSpec where
    readsPrec _ "EIGENSTRAT" = [(GenotypeOutFormatEigenstrat, "")]
    readsPrec _ "PLINK"      = [(GenotypeOutFormatPlink,      "")]
    readsPrec _ "VCF"        = [(GenotypeOutFormatVCF ,       "")]
    readsPrec _ _            = []

-- | To facilitate automatic parsing of GenotypeDataSpec from JSON files
instance FromJSON GenotypeDataSpec where
    parseJSON = withObject "GenotypeData" $ \v -> do
        gformat <- v .: "format"
        gfileSpec <- case gformat of
            "EIGENSTRAT" -> GenotypeEigenstrat
                <$> v .:  "genoFile"
                <*> v .:? "genoFileChkSum"
                <*> v .:  "snpFile"
                <*> v .:? "snpFileChkSum"
                <*> v .:  "indFile"
                <*> v .:? "indFileChkSum"
            "PLINK" -> GenotypePlink
                <$> v .:  "genoFile"
                <*> v .:? "genoFileChkSum"
                <*> v .:  "snpFile"
                <*> v .:? "snpFileChkSum"
                <*> v .:  "indFile"
                <*> v .:? "indFileChkSum"
            "VCF" -> GenotypeVCF
                <$> v .:  "genoFile"
                <*> v .:? "genoFileChkSum"
            _ -> fail ("unknown format " ++ T.unpack gformat)
        snpSet <- v .:? "snpSet"
        refName <- v .:? "referenceGenomeAssembly"
        refURL  <- v .:? "referenceGenomeAssemblyURL"
        return $ GenotypeDataSpec gfileSpec snpSet refName refURL

instance ToJSON GenotypeDataSpec where
    -- this encodes directly to a bytestring Builder
    toJSON (GenotypeDataSpec gfileSpec snpSet refName refURL) = case gfileSpec of
        GenotypeEigenstrat genoF genoFchk snpF snpFchk indF indFchk ->
            object [
                "format"        .= ("EIGENSTRAT" :: String),
                "genoFile"      .= genoF,
                "genoFileChkSum".= genoFchk,
                "snpFile"       .= snpF,
                "snpFileChkSum" .= snpFchk,
                "indFile"       .= indF,
                "indFileChkSum" .= indFchk,
                "snpSet"        .= snpSet,
                "referenceGenomeAssembly"    .= refName,
                "referenceGenomeAssemblyURL" .= refURL
            ]
        GenotypePlink genoF genoFchk snpF snpFchk indF indFchk ->
            object [
                "format"        .= ("PLINK" :: String),
                "genoFile"      .= genoF,
                "genoFileChkSum".= genoFchk,
                "snpFile"       .= snpF,
                "snpFileChkSum" .= snpFchk,
                "indFile"       .= indF,
                "indFileChkSum" .= indFchk,
                "snpSet"        .= snpSet,
                "referenceGenomeAssembly"    .= refName,
                "referenceGenomeAssemblyURL" .= refURL
            ]
        GenotypeVCF genoF genoFchk ->
            object [
                "format"        .= ("VCF" :: String),
                "genoFile"      .= genoF,
                "genoFileChkSum".= genoFchk,
                "snpSet"        .= snpSet,
                "referenceGenomeAssembly"    .= refName,
                "referenceGenomeAssemblyURL" .= refURL
            ]

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

-- | removes directories of all filenames and returns a tuple of the basename and a modified GenotypeDataSpec with pure filenames
-- In case basedirectories do not match, this function will throw an exception
reduceGenotypeFilepaths :: (MonadThrow m) => GenotypeDataSpec -> m (FilePath, GenotypeDataSpec)
reduceGenotypeFilepaths gd@(GenotypeDataSpec gFileSpec _ _ _) = do
    (baseDir, newGfileSpec) <- case gFileSpec of
        GenotypeEigenstrat genoF _ snpF _ indF _ -> do
            let baseDirs  = map takeDirectory   [genoF, snpF, indF]
                fileNames = map takeFileName [genoF, snpF, indF]
            unless (all (==(head baseDirs)) baseDirs) . throwM $ PoseidonUnequalBaseDirException genoF snpF indF
            return (head baseDirs, gFileSpec {_esGenoFile = fileNames !! 0, _esSnpFile = fileNames !! 1, _esIndFile = fileNames !! 2})
        GenotypePlink genoF _ snpF _ indF _ -> do
            let baseDirs  = map takeDirectory   [genoF, snpF, indF]
                fileNames = map takeFileName [genoF, snpF, indF]
            unless (all (==(head baseDirs)) baseDirs) . throwM $ PoseidonUnequalBaseDirException genoF snpF indF
            return (head baseDirs, gFileSpec {_plGenoFile = fileNames !! 0, _plSnpFile = fileNames !! 1, _plIndFile = fileNames !! 2})
        GenotypeVCF genoF _ -> do
            let baseDir  = takeDirectory   genoF
                fileName = takeFileName genoF
            return (baseDir, gFileSpec {_vcfGenoFile = fileName})
    return (baseDir, gd {genotypeFileSpec = newGfileSpec})

-- | A function to return a list of all individuals in the genotype files of a package.
loadIndividuals :: FilePath -- ^ the base directory
               -> GenotypeDataSpec -- ^ the Genotype spec
               -> PoseidonIO [EigenstratIndEntry] -- ^ the returned list of EigenstratIndEntries.
loadIndividuals d (GenotypeDataSpec gFileSpec _ _ _) = do
    popMode <- envInputPlinkMode
    case gFileSpec of
        GenotypeEigenstrat _ _ _ _ fn _ -> readEigenstratInd (d </> fn)
        GenotypePlink _ _ _ _ fn _      -> map (plinkFam2EigenstratInd popMode) <$> readFamFile (d </> fn)
        GenotypeVCF fn _ -> do
            (VCFheader headerLines sampleNames , _) <- liftIO . runSafeT . readVCFfromFile $ (d </> fn)
            -- we try to read the group names and sex entries from the VCF header.
            groupNames <- case findGroupNamesInVCFheader headerLines of
                    Nothing -> return $ replicate (length sampleNames) "unknown"
                    Just gn -> do
                        when (length gn /= length sampleNames) $
                            throwM . PoseidonGenotypeException $ "Number of group names (" ++ show gn ++ ") in VCF header does not match number of samples (" ++ show sampleNames ++ ")"
                        return gn
            geneticSex <- case findGeneticSexInVCFheader headerLines of
                    Nothing -> return $ replicate (length sampleNames) Unknown
                    Just (Left err) ->
                        throwM . PoseidonGenotypeException $ "Error in parsing genetic sex entries from VCF header: " ++ err
                    Just (Right gs) -> do
                        when (length gs /= length sampleNames) $
                            throwM . PoseidonGenotypeException $ "Number of genetic sex entries (" ++ show gs ++ ") in VCF header does not match number of samples (" ++ show sampleNames ++ ")"
                        return gs
            return [EigenstratIndEntry s gs gn | (s, gs, gn) <- zip3 sampleNames geneticSex groupNames]

findGroupNamesInVCFheader :: [B.ByteString] -> Maybe [B.ByteString]
findGroupNamesInVCFheader headerLines = case filter ("##group_names=" `B.isPrefixOf`) headerLines of
    []    -> Nothing
    (l:_) -> Just . B.split ',' . B.drop 14 $ l

findGeneticSexInVCFheader :: [B.ByteString] -> Maybe (Either String [Sex])
findGeneticSexInVCFheader headerLines = case filter ("##genetic_sex=" `B.isPrefixOf`) headerLines of
    [] -> Nothing
    (l:_) -> case A.parseOnly (A.string "##genetic_sex=" *> parseSex `A.sepBy` A.char ',') l of
        Left err -> Just (Left $ "When parsing genetic sex entries (" ++ B.unpack l ++ ") encountered error: " ++ err)
        Right r -> Just (Right r)

-- | A function to read the genotype data of a package
loadGenotypeData :: (MonadSafe m) =>
                   FilePath -- ^ the base path
                -> GenotypeDataSpec -- ^ the genotype spec
                -> m (Producer (EigenstratSnpEntry, GenoLine) m ())
                -- ^ a Producer over the Snp position values and the genotype line.
loadGenotypeData baseDir (GenotypeDataSpec gFileSpec _ _ _) =
    case gFileSpec of
        GenotypeEigenstrat genoF _ snpF _ indF _ -> snd <$> readEigenstrat (baseDir </> genoF) (baseDir </> snpF) (baseDir </> indF)
        GenotypePlink      genoF _ snpF _ indF _ -> snd <$> readPlink (baseDir </> genoF) (baseDir </> snpF) (baseDir </> indF)
        GenotypeVCF fn _ -> do
            vcfProd <- snd <$> readVCFfromFile (baseDir </> fn)
            return $ vcfProd >-> vcf2eigenstratPipe

vcf2eigenstratPipe :: (MonadThrow m) => Pipe VCFentry (EigenstratSnpEntry, GenoLine) m r
vcf2eigenstratPipe = for cat $ \vcfEntry -> do
    -- freqSum is a useful intermediate format.
    -- vcfToFreqSumEntry already does a bunch of checks of the VCF data.
    (FreqSumEntry chrom pos snpId_ geneticPos ref alt alleleCounts) <- vcfToFreqSumEntry vcfEntry
    let eigenstratSnpEntry = EigenstratSnpEntry chrom pos (fromMaybe 0.0 geneticPos) (fromMaybe "" snpId_) ref alt
    genoLine <- V.fromList <$> forM alleleCounts (\dosage -> do
        case dosage of
            Nothing     -> return Missing
            Just (0, 1) -> return HomRef
            Just (1, 1) -> return HomAlt
            Just (0, 2) -> return HomRef
            Just (1, 2) -> return Het
            Just (2, 2) -> return HomAlt
            _ -> throwM . PoseidonGenotypeException $
                "illegal dosage in VCF file! Make sure genotypes in your VCF \
                \file are biallelic and either haploid or diploid")
    yield (eigenstratSnpEntry, genoLine)

joinEntries :: [Int] -> [String] -> Bool -> [Maybe (EigenstratSnpEntry, GenoLine)] -> Either String (Maybe (EigenstratSnpEntry, GenoLine))
joinEntries nrInds pacNames strandCheck maybeTupleList = do
    let allSnpEntries = map fst . catMaybes $ maybeTupleList
    case getConsensusSnpEntry strandCheck allSnpEntries of
        Nothing -> Right Nothing -- No valid consensus SNP entry in the case of strandcheck, either due to strand-ambiguous alleles or if non non-missing allele pairs are present.
        Just consensusSnpEntry -> do
            recodedGenotypes <- forM (zip3 nrInds pacNames maybeTupleList) $ \(n, name, maybeTuple) -> do
                case maybeTuple of
                    Nothing -> return $ V.replicate n Missing
                    Just (snpEntry, genoLine) -> case checkAlleleFlipNeeded consensusSnpEntry snpEntry strandCheck genoLine of
                        Left err -> Left $ "Encountered inconsistent alleles in package " ++ name ++ ": " ++ err
                        Right alleleFlipStatus -> return $ recodeAlleles alleleFlipStatus genoLine
            return $ Just (consensusSnpEntry, V.concat recodedGenotypes)

getConsensusSnpEntry :: Bool -> [EigenstratSnpEntry] -> Maybe EigenstratSnpEntry
getConsensusSnpEntry strandCheck snpEntries = do
    let chrom = snpChrom . head $ snpEntries
        pos = snpPos . head $ snpEntries
        uniqueIds = nub . map snpId $ snpEntries
        uniqueGenPos = sort . nub . map snpGeneticPos $ snpEntries
    id_ <- case uniqueIds of
        [i] -> return i
        _ -> do -- multiple Ids: Picking the first rs-number if possible, otherwise the first one.
            let rsIds = filter (isPrefixOf "rs") uniqueIds
            case rsIds of
                (i:_) -> return i
                _     -> return $ head uniqueIds
    genPos <- case uniqueGenPos of
        [p]      -> return p
        [0.0, p] -> return p -- 0.0 is considered "no data" in genetic position column
        _        -> return $ maximum uniqueGenPos -- multiple non-zero genetic positions. Choosing the largest one.
    (ref, alt) <- if strandCheck then getConsensusAllelesStrandCheck snpEntries else return (getConsensusAlleles snpEntries)
    return (EigenstratSnpEntry chrom pos genPos id_ ref alt)

missingAlleles :: [Char]
missingAlleles = ['N', '0', 'X']

isMissing :: Char -> Bool
isMissing a = a `elem` missingAlleles

-- this algorithm simply collects two different non-Missing alleles and declares them to be the consensus alleles,
-- filling up with Ns if need be.
getConsensusAlleles :: [EigenstratSnpEntry] -> (Char, Char)
getConsensusAlleles snpEntries =
    let allAlleles    = concat $ [[r, a] | EigenstratSnpEntry _ _ _ _ r a <- snpEntries]
        uniqueAlleles = nub . filter (not . isMissing) $ allAlleles
    in  case uniqueAlleles of
            []              ->  ('N', 'N')
            [r]             -> ('N', r)
            (ref : alt : _) -> (ref, alt) -- at this point we don't care if we have more than two alleles,
                -- this will be checked in the Recoding step.

-- this is a different algorithm: If strand flips are allowed, we need to have at least one non-missing allele-pair within
-- one genotype source. In addition we cannot tolerate strand-ambiguous pairs (C/G and A/T). So this may return a Nothing.
getConsensusAllelesStrandCheck :: [EigenstratSnpEntry] -> Maybe (Char, Char)
getConsensusAllelesStrandCheck snpEntries = do
    let allAllelePairs    = [(r, a) | EigenstratSnpEntry _ _ _ _ r a <- snpEntries]
    case filter (\(a, b) -> not (isMissing a) && not (isMissing b)) $ allAllelePairs of
        [] -> Nothing -- no non-missing allele pairs, we cannot determine the consensus alleles.
        ((a, b):_) -> if (a, b) `elem` [('C', 'G'), ('G', 'C'), ('A', 'T'), ('T', 'A')]
            then Nothing -- Strand-ambiguous allele pair, we cannot determine the consensus alleles.
            else return (a, b)

checkAlleleFlipNeeded :: EigenstratSnpEntry -> EigenstratSnpEntry -> Bool -> GenoLine -> Either String Bool
checkAlleleFlipNeeded
            (EigenstratSnpEntry chrom pos _ snpId_ consRefA consAltA)
            (EigenstratSnpEntry _ _ _ _ refA altA)
            strandCheck
            genoLine =
    -- we begin by homogenising missing alleles to 'N' to help equality checks.
    let refA' = if isMissing refA then 'N' else refA
        altA' = if isMissing altA then 'N' else altA
        consRefA' = if isMissing consRefA then 'N' else consRefA
        consAltA' = if isMissing consAltA then 'N' else consAltA
    in
        if not . any isMissing $ [refA, altA, consRefA, consAltA] then (
            if (refA', altA') == (consRefA', consAltA') then Right False -- simple concordance
                else if (refA', altA') == (consAltA', consRefA') then Right True  -- alleles flipped
                else if strandCheck && (revComp refA', revComp altA') == (consRefA', consAltA') then
                    -- simple concordance on reverse strand
                    Right False
                else if strandCheck && (revComp refA', revComp altA') == (consAltA', consRefA') then
                    -- alleles flipped on reverse strand
                    Right True
                else inconsistent
        )
        else (
            -- We are OK with a partial match in case of missingness and as
            -- long as the genotypes are then all homRef or homAlt, or missing, respectively.
            if refA' == consRefA' || (strandCheck && revComp refA' == consRefA') then do
                validateAllRef
                Right False
            else if altA' == consAltA' || (strandCheck && revComp altA' == consAltA') then do
                validateAllAlt
                Right False
            else if refA' == consAltA' || (strandCheck && revComp refA' == consAltA') then do
                validateAllRef
                Right True
            else if altA' == consRefA' || (strandCheck && revComp altA' == consRefA') then do
                validateAllAlt
                Right True
            else
                inconsistent
        )
  where
    validateAllRef = unless (V.all (\a -> a == HomRef || a == Missing) genoLine) inconsistent
    validateAllAlt = unless (V.all (\a -> a == HomAlt || a == Missing) genoLine) inconsistent
    inconsistent = Left $ "inconsistent alleles for SNP " ++ show snpId_ ++ " at position " ++ show chrom ++ ":" ++ show pos ++
                ". Consensus alleles inferred from all input packages are " ++ [consRefA, consAltA] ++
                ", but package has alleles " ++ [refA, altA] ++ ". Could this be due to strand-flips? Consider using the --strandcheck option to check for strand flips and to automatically flip strands if needed."
    revComp 'A' = 'T'
    revComp 'T' = 'A'
    revComp 'C' = 'G'
    revComp 'G' = 'C'
    revComp x   = x

recodeAlleles :: Bool -> GenoLine -> GenoLine
recodeAlleles False genoLine = genoLine
recodeAlleles True  genoLine = V.map flipGeno genoLine
  where
    flipGeno HomRef = HomAlt
    flipGeno HomAlt = HomRef
    flipGeno g      = g

printSNPCopyProgress :: (MonadIO m) => LogA -> UTCTime -> Pipe a a m ()
printSNPCopyProgress logA startTime = do
    counterRef <- liftIO $ newIORef (0 :: Int)
    for cat $ \val -> do
        n <- liftIO $ readIORef counterRef
        currentTime <- liftIO getCurrentTime
        logWithEnv logA $ logProgress n (diffUTCTime currentTime startTime)
        liftIO $ modifyIORef counterRef (+1)
        yield val
    where
        logProgress :: Int -> NominalDiffTime -> PoseidonIO ()
        logProgress c t
            |  c `rem` 10000 == 0 = logInfo $ "SNPs: " ++ padLeft 9 (show c) ++ "    " ++ prettyTime (floor t)
            |  c == 1000          = logInfo $ "Probing of the first 1000 SNPs successful. Continuing now..."
            | otherwise = return ()
        prettyTime :: Int -> String
        prettyTime t
            | t < 60 = show t ++ "s"
            | t >= 60 && t < 3600 = do
                let (minutes, rest) = t `quotRem` 60
                show minutes ++ "m " ++ prettyTime rest
            | otherwise = do
                let (hours, rest) = t `quotRem` 3600
                show hours   ++ "h " ++ prettyTime rest


selectIndices :: [Int] -> (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine)
selectIndices indices (snpEntry, genoLine) = (snpEntry, V.fromList [genoLine V.! i | i <- indices])

writeVCF :: (MonadSafe m) => LogA -> [JannoRow] -> FilePath -> Consumer (EigenstratSnpEntry, GenoLine) m ()
writeVCF logA jannoRows vcfFile = do
    let sampleNames = map (unPoseidonID . jPoseidonID) jannoRows
        groupNames  = map (B.unpack . unGroupName . head . getListColumn . jGroupName) jannoRows
        sex         = map jGeneticSex jannoRows
    forM_ jannoRows $ \jannoRow -> do
        when (jGenotypePloidy jannoRow == Nothing) . logWithEnv logA . logWarning $
            "Missing GenotypePloidy for individual " ++ show (jPoseidonID jannoRow) ++
            ". For VCF output I will assume diploid genotypes. " ++
            "Please set the GenotypePloidy column explitly in the Janno File to haploid or diploid."
    let metaInfoLines = map B.pack [
            "##fileformat=VCFv4.2",
            "##source=trident_v" ++ showVersion version,
            "##group_names=" ++ intercalate "," groupNames,
            "##genetic_sex=" ++ (intercalate "," . map show) sex,
            "##INFO=<ID=NS,Number=1,Type=Integer,Description=\"Number of Samples With Data\">",
            "##FILTER=<ID=s50,Description=\"Less than 50% of samples have data\">",
            "##FILTER=<ID=s10,Description=\"Less than 10% of samples have data\">",
            "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">"]
        vcfh = VCFheader metaInfoLines sampleNames
    P.mapM (liftIO . createVCFentry logA jannoRows) >-> writeVCFfile vcfFile vcfh

createVCFentry :: (MonadIO m) => LogA -> [JannoRow] -> (EigenstratSnpEntry, GenoLine) -> m VCFentry
createVCFentry logA jannoRows (EigenstratSnpEntry chrom pos _ id_ ref alt, genoLine) = do
    gt <- genotypes
    return $ VCFentry chrom pos (Just id_) (B.pack [ref]) altField Nothing (Just filterString)
            infoFields (Just (["GT"], gt))
  where
    altField = if alt == 'N' then [] else [B.pack [alt]]
    nrMissing = V.length . V.filter (==Missing) $ genoLine
    nrSamples = V.length genoLine
    filterString
        | nrMissing * 10 > 9 * nrSamples = "s10;s50"
        | nrMissing * 2 > nrSamples      = "s50"
        | otherwise                      = "PASS"
    nrSamplesWithData = nrSamples - nrMissing
    infoFields = [B.pack ("NS=" ++ show nrSamplesWithData)]
    genotypes = forM (zip3 sampleNames genoEntries ploidyList) $ \(s, g, p) -> case (g, p) of
        (Missing, Just Haploid) -> return ["."]
        (HomRef , Just Haploid) -> return ["0"]
        (Het    , Just Haploid) -> do
            logWithEnv logA . logWarning $ "Encountered a heterozygous genotype for " ++ show (unPoseidonID s) ++
                " at position " ++ show chrom ++ ":" ++ show pos ++ ", but the individual's GenotypePloidy is given as " ++
                " Haploid in the Janno-File. I have to encode this in the VCF as a diploid genotype. Consider changing this " ++
                "individual's GenotypePloidy to diploid!"
            return ["0/1"]
        (HomAlt , Just Haploid) -> return ["1"]
        (Missing, Just Diploid) -> return ["./."]
        (HomRef , Just Diploid) -> return ["0/0"]
        (Het    , Just Diploid) -> return ["0/1"]
        (HomAlt , Just Diploid) -> return ["1/1"]
        (Missing, Nothing)      -> return ["./."] -- we assume diploid in these case, see warning above in
        (HomRef , Nothing)      -> return ["0/0"]
        (Het    , Nothing)      -> return ["0/1"]
        (HomAlt , Nothing)      -> return ["1/1"]
    sampleNames = map jPoseidonID jannoRows
    genoEntries = V.toList genoLine
    ploidyList = map jGenotypePloidy jannoRows
