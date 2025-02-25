{-# LANGUAGE OverloadedStrings #-}
module Poseidon.GenotypeData where

import           Paths_poseidon_hs          (version)
import           Poseidon.Janno             (GroupName (..),
                                             JannoGenotypePloidy (..),
                                             JannoRow (jGenotypePloidy, jGroupName, jPoseidonID),
                                             ListColumn (..))
import           Poseidon.Utils             (LogA, PoseidonException (..),
                                             PoseidonIO, checkFile,
                                             envInputPlinkMode, logDebug,
                                             logInfo, logWarning, logWithEnv,
                                             padLeft)

import           Control.Exception          (throwIO)
import           Control.Monad              (forM, forM_, unless, when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.:), (.:?), (.=))
import           Data.ByteString            (isPrefixOf)
import qualified Data.ByteString.Char8      as B
import           Data.IORef                 (modifyIORef, newIORef, readIORef)
import           Data.List                  (intercalate, nub, sort)
import           Data.Maybe                 (catMaybes, fromMaybe)
import qualified Data.Text                  as T
import           Data.Time                  (NominalDiffTime, UTCTime,
                                             diffUTCTime, getCurrentTime)
import qualified Data.Vector                as V
import           Data.Version               (showVersion)
import           Pipes                      (Consumer, Pipe, Producer, cat, for,
                                             yield, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (MonadSafe, runSafeT)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine, Sex (..),
                                             readEigenstrat, readEigenstratInd)
import           SequenceFormats.FreqSum    (FreqSumEntry (..))
import           SequenceFormats.Plink      (plinkFam2EigenstratInd,
                                             readFamFile, readPlink)
import           SequenceFormats.VCF        (VCFentry (..), VCFheader (..),
                                             readVCFfromFile, vcfToFreqSumEntry,
                                             writeVCFfile)
import           System.FilePath            (takeDirectory, takeFileName, (</>))

data GenoDataSource = PacBaseDir
    { getPacBaseDir :: FilePath
    }
    | GenoDirect
    { getGenoDirect :: GenotypeDataSpec
    }
    deriving Show

data GenotypeDataSpec = GenotypeDataSpec {
    genotypeFileSpec :: GenotypeFileSpec,
    genotypeSnpSet   :: Maybe SNPSetSpec
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
        return $ GenotypeDataSpec gfileSpec snpSet

instance ToJSON GenotypeDataSpec where
    -- this encodes directly to a bytestring Builder
    toJSON (GenotypeDataSpec gfileSpec snpSet) = case gfileSpec of
        GenotypeEigenstrat genoF genoFchk snpF snpFchk indF indFchk ->
            object [
                "format"        .= ("EIGENSTRAT" :: String),
                "genoFile"      .= genoF,
                "genoFileChkSum".= genoFchk,
                "snpFile"       .= snpF,
                "snpFileChkSum" .= snpFchk,
                "indFile"       .= indF,
                "indFileChkSum" .= indFchk,
                "snpSet"        .= snpSet
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
                "snpSet"        .= snpSet
            ]
        GenotypeVCF genoF genoFchk ->
            object [
                "format"        .= ("VCF" :: String),
                "genoFile"      .= genoF,
                "genoFileChkSum".= genoFchk
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
reduceGenotypeFilepaths gd@(GenotypeDataSpec gFileSpec _) = do
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
loadIndividuals d (GenotypeDataSpec gFileSpec _) = do
    popMode <- envInputPlinkMode
    case gFileSpec of
        GenotypeEigenstrat _ _ _ _ fn fnChk -> do
            liftIO $ checkFile (d </> fn) fnChk
            readEigenstratInd (d </> fn)
        GenotypePlink _ _ _ _ fn fnChk -> do
            liftIO $ checkFile (d </> fn) fnChk
            map (plinkFam2EigenstratInd popMode) <$> readFamFile (d </> fn)
        GenotypeVCF fn fnChk -> do
            liftIO $ checkFile (d </> fn) fnChk
            (VCFheader _ sampleNames , _) <- liftIO . runSafeT . readVCFfromFile $ (d </> fn)
            --neither Sex nor population name is part of a VCF file, so we fill dummy values:
            return [EigenstratIndEntry s Unknown "unknown" | s <- sampleNames]

-- | A function to read the genotype data of a package
loadGenotypeData :: (MonadSafe m) =>
                   FilePath -- ^ the base path
                -> GenotypeDataSpec -- ^ the genotype spec
                -> m (Producer (EigenstratSnpEntry, GenoLine) m ())
                -- ^ a Producer over the Snp position values and the genotype line.
loadGenotypeData baseDir (GenotypeDataSpec gFileSpec _) =
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

joinEntries :: (MonadIO m) => LogA -> [Int] -> [String] -> [Maybe (EigenstratSnpEntry, GenoLine)] -> m (EigenstratSnpEntry, GenoLine)
joinEntries logA nrInds pacNames maybeTupleList = do
    let allSnpEntries = map fst . catMaybes $ maybeTupleList
    consensusSnpEntry <- getConsensusSnpEntry logA allSnpEntries
    recodedGenotypes <- forM (zip3 nrInds pacNames maybeTupleList) $ \(n, name, maybeTuple) ->
        case maybeTuple of
            Nothing -> return (V.replicate n Missing)
            Just (snpEntry, genoLine) -> case recodeAlleles consensusSnpEntry snpEntry genoLine of
                Left err -> do
                    let msg = "Error in genotype data of package " ++ name ++ ": " ++ err
                    liftIO . throwIO $ PoseidonGenotypeException msg
                Right x -> return x
    return (consensusSnpEntry, V.concat recodedGenotypes)

getConsensusSnpEntry :: (MonadIO m) => LogA -> [EigenstratSnpEntry] -> m EigenstratSnpEntry
getConsensusSnpEntry logA snpEntries = do
    let chrom = snpChrom . head $ snpEntries
        pos = snpPos . head $ snpEntries
        uniqueIds = nub . map snpId $ snpEntries
        uniqueGenPos = sort . nub . map snpGeneticPos $ snpEntries
        allAlleles    = concat $ [[r, a] | EigenstratSnpEntry _ _ _ _ r a <- snpEntries]
        uniqueAlleles = nub . filter (\a -> a /= 'N' && a /= '0' && a /= 'X') $ allAlleles
    id_ <- case uniqueIds of
        [i] -> return i
        _ -> do -- multiple Ids: Picking the first rs-number if possible, otherwise the first one.
            let rsIds = filter (isPrefixOf "rs") uniqueIds
                selectedId = case rsIds of
                    (i:_) -> i
                    _     -> head uniqueIds
            logWithEnv logA . logDebug $
                "Found inconsistent SNP IDs: " ++ show uniqueIds ++ ". Choosing " ++ show selectedId
            return selectedId
    genPos <- case uniqueGenPos of
        [p] -> return p
        [0.0, p] -> return p -- 0.0 is considered "no data" in genetic position column
        _ -> do -- multiple non-zero genetic positions. Choosing the largest one.
            let selectedGenPos = maximum uniqueGenPos
            logWithEnv logA . logDebug $
                "Found inconsistent genetic positions in SNP " ++ show id_ ++ ": " ++
                show uniqueGenPos ++ ". Choosing " ++ show selectedGenPos
            return selectedGenPos
    case uniqueAlleles of
        [] -> do -- no non-missing alleles found
            -- logWithEnv LogA . logDebug $
            --     "SNP " ++ show id_ ++ " appears to have no data (both ref and alt allele are blank"
            return (EigenstratSnpEntry chrom pos genPos id_ 'N' 'N')
        [r] -> do -- only one non-missing allele found
            -- logWithEnv LogA . logDebug $
            --     "SNP " ++ show id_ ++ " appears to be monomorphic (only one of ref and alt alleles are non-blank)"
            return (EigenstratSnpEntry chrom pos genPos id_ 'N' r)
        [ref, alt] ->
            return (EigenstratSnpEntry chrom pos genPos id_ ref alt)
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
    let sampleNames = map (B.pack . jPoseidonID) jannoRows
        groupNames  = map ((\(GroupName n) -> T.unpack n) . head . getListColumn . jGroupName) jannoRows
    forM_ jannoRows $ \jannoRow -> do
        when (jGenotypePloidy jannoRow == Nothing) . logWithEnv logA . logWarning $
            "Missing GenotypePloidy for individual ++ " ++ jPoseidonID jannoRow ++
            ". For VCF output I will assume diploid genotypes. " ++
            "Please set the GenotypePloidy column explitly in the Janno File to Haploid or Diploid."
    let metaInfoLines = map B.pack [
            "##fileformat=VCFv4.2",
            "##source=trident_v" ++ showVersion version,
            "##group_names=" ++ intercalate "," groupNames,
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
            logWithEnv logA . logWarning $ "Encountered a heterozygous genotype for " ++ s ++
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
