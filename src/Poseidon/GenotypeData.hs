{-# LANGUAGE OverloadedStrings #-}
module Poseidon.GenotypeData where

import           Paths_poseidon_hs          (version)
import           Poseidon.Utils             (LogA, PoseidonException (..),
                                             PoseidonIO, checkFile,
                                             envInputPlinkMode, logDebug,
                                             logInfo, logWithEnv, padLeft)

import           Control.Exception          (throwIO)
import           Control.Monad              (forM, unless)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON, ToJSON, object,
                                             parseJSON, toJSON, withObject,
                                             withText, (.:), (.:?), (.=))
import           Data.ByteString            (isPrefixOf)
import qualified Data.ByteString.Char8      as B
import           Data.IORef                 (modifyIORef, newIORef, readIORef)
import           Data.List                  (intercalate, nub, sort)
import           Data.Maybe                 (catMaybes)
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
                                             readVCFfromFile, vcfToFreqSumEntry)
import           System.Environment         (getArgs, getProgName)
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

vcf2eigenstratPipe :: (PoseidonIO m) => [JannoRows] -> Pipe VCFentry (EigenstratSnpEntry, GenoLine) m r
vcf2eigenstratPipe jannowRows = for cat $ \vcfEntry -> do
    case vcfToFreqSumEntry vcfEntry of -- freqSum is a useful intermediate format.
                                       -- vcfToFreqSumEntry already does a bunch of checks of the VCF data.
        Right (FreqSumEntry chrom pos snpId_ geneticPos ref alt alleleCounts) -> do
            let eigenstratSnpEntry = EigenstratSnpEntry chrom pos (maybe 0.0 id geneticPos) (maybe "" id snpId_) ref alt
            genoLine <- V.fromList <$> forM [0 .. (length alleleCounts - 1)] (\i -> do
                let sampleName = jPoseidonID $ jannoRows !! i
                let dosage = alleleCounts !! i
                let ploidy = jGenotypePloidy $ jannoRows !! i
                case (dosage, ploidy) of
                    (Nothing, _) -> return Missing
                    (Just (0, 1), Haploid) -> return HomRef
                    (Just (1, 1), Haploid) -> return HomAlt
                    (Just (_, 1), Diploid) -> do
                        logWarning $ "For sample " ++ sampleName ++ " encountered haploid genotype at " ++
                            show chrom ++ ":" ++ show pos ++ " despite Diploid annotation for GenotypePloidy in the Janno file. \
                            \I will have to set this as missing."
                            return Missing
                    (Just (0, 2), Diploid) -> return HomRef
                    (Just (1, 2), Diploid) -> return Het
                    (Just (2, 2), Diploid) -> return HomAlt
                    (Just (_, 2), Haploid) -> do
                        logWarning $ "For sample " ++ sampleName ++ " encountered diploid genotype at " ++
                            show chrom ++ ":" ++ show pos ++ " despite Haploid annotation for GenotypePloidy in the Janno file. \
                            \I will have to set this as missing."
                            return Missing
                    _ -> liftIO . throwIO .PoseidonGenotypeException $
                            "illegal dosage for sample " ++ jPoseidonID (jannoRows !! i) ++
                            "at " ++ show alleleCount ++ ") in VCF file at " ++ show chrom ++ ":" ++ show pos
            yield (eigenstratSnpEntry, genoLine)
        Left err -> liftIO . throwIO . PoseidonGenotypeException $ err

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

writeVCF :: (MonadSafe m) => FilePath -> [EigenstratIndEntry] -> Consumer (EigenstratSnpEntry, GenoLine) m ()
writeVCF vcfFile eigenstratIndEntries = do
    let sampleNames = [n | EigenstratIndEntry n _ _ <- eigenstratIndEntries]
        groupNames  = [g | EigenstratIndEntry _ _ g <- eigenstratIndEntries]
    prog_name <- liftIO getProgName
    prog_args <- liftIO getArgs
    let command_line = prog_name ++ " " ++ intercalate " " prog_args
    let metaInfoLines = map B.pack [
            "##fileformat=VCFv4.2",
            "##source=trident_v" ++ showVersion version,
            "##command_line=" ++ command_line,
            "##group_names=" ++ intercalate "," groupNames,
            "##INFO=<ID=NS,Number=1,Type=Integer,Description=\"Number of Samples With Data\">",
            "##INFO=<ID=AF,Number=A,Type=Float,Description=\"Allele Frequency\">",
            "##FILTER=<ID=s50,Description=\"Less than 50% of samples have data\">",
            "##FILTER=<ID=s10,Description=\"Less than 10% of samples have data\">",
            "##FORMAT=<ID=GT,Number=1,Type=String,Description=\"Genotype\">"]
        vcfh = VCFheader metaInfoLines sampleNames
    P.mapM createVCFentry >-> writeVCFfile vcfFile vcfh

createVCFentry :: [JannoRow] -> (EigenstratSnpEntry, GenoLine) -> PoseidonIO VCFentry
createVCFentry jannoRows (EigenstratSnpEntry chrom pos _ id_ ref alt, genoLine) = do
    gt <- genotypes
    return $ VCFentry chrom pos (Just id_) (B.pack [ref]) [B.pack [alt]] Nothing (Just filterString)
             infoFields (Just ["GT"], gt)
  where
    nrMissing = V.length . V.filter (==Missing) $ genoLine
    nrSamples = V.length genoLine
    filterString =
        if nrMissing * 10 > 9 * nrSamples then "s10;s50"
        else if nrMissing * 2 > nrSamples then "s50"
        else "PASS"
    nrSamplesWithData = nrSamples - nrMissing
    alleleFreq = computeAlleleFreq genoLine
    infoFields = [
        B.pack $ "NS=" ++ show nrSamplesWithData] ++ 
        case alleleFreq of
            Just f ->
                let roundedFreq = fromIntegral (round (f * 100.0)) / 100.0 :: Double
                in  [B.pack $ "AF=" ++ show roundedFreq]
            Nothing -> []
    genotypes = forM [0 .. (nrSamples - 1)] $ \i -> case (genoLine V.! i, ploidyVec V.! i) of
        (Missing, Haploid) -> return "."
        (HomRef , Haploid) -> return "0"
        (Het    , Haploid) -> do
            let sampleName = jPoseidonID $ jannoRows !! i
            logWarning $ "Encountered a heterozygous genotype for " ++ sampleName ++
                " at position " ++ chrom ++ ":" ++ pos ++ ", but the individual's GenotypePloidy is " ++
                " haploid in the Janno-File. I have to encode this in the VCF as a diploid genotype. Consider changing this " ++
                "individual's GenotypePloidy to diploid!"
            return "0/1"
        (HomAlt , Haploid) -> return "1"
        (Missing, Diploid) -> return "./."
        (HomRef , Diploid) -> return "0/0"
        (Het    , Diploid) -> return "0/1"
        (HomAlt , Diploid) -> return "1/1"

computeAlleleFreq :: GenoLine -> Maybe Double
computeAlleleFreq genoLine =
    let nrTotalAlleles = sum . map (maybe 0 snd) $ dosages
        nrNonRefAlleles = sum . map (maybe 0 fst) $ dosages
    in  if nrTotalAlleles == 0 then Nothing else
            Just (fromIntegral nrNonRefAlleles / fromIntegral nrTotalAlleles)
