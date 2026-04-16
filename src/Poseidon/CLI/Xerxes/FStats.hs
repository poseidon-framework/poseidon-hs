{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Analysis.CLI.FStats (
      FStatSpec(..)
    , P.ParseError
    , P.runParser
    , FstatsOptions(..)
    , JackknifeMode(..)
    , runFstats,
    collectStatSpecGroups
) where

import           Poseidon.Analysis.FStatsConfig (AscertainmentSpec (..),
                                                 FStatInput, FStatSpec (..),
                                                 FStatType (..), readFstatInput)
import           Poseidon.Analysis.Utils        (GenomPos, JackknifeMode (..),
                                                 PloidyVec,
                                                 XerxesException (..),
                                                 addGroupDefs,
                                                 computeAlleleCount,
                                                 computeAlleleFreq,
                                                 computeJackknifeOriginal,
                                                 filterTransitions,
                                                 makePloidyVec)

import           Control.Exception              (catch, throwIO)
import           Control.Foldl                  (FoldM (..), impurely, list,
                                                 purely)
import           Control.Monad                  (forM, forM_, unless, when)
import           Control.Monad.Catch            (throwM)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.IORef                     (IORef, modifyIORef', newIORef,
                                                 readIORef, writeIORef)
import           Data.List                      (intercalate, nub, (\\))
import qualified Data.Map                       as M
import           Data.Maybe                     (fromMaybe)
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import qualified Data.Vector.Unboxed.Mutable    as VUM
-- import           Debug.Trace                 (trace)
import           Lens.Family2                   (view)
import           Pipes                          (cat, for, yield, (>->))
import           Pipes.Group                    (chunksOf, foldsM, groupsBy)
import qualified Pipes.Prelude                  as P
import           Pipes.Safe                     (runSafeT)
import           Poseidon.ColumnTypesJanno      (JannoGenotypePloidy (..))
import           Poseidon.EntityTypes           (IndividualInfo (..),
                                                 PoseidonEntity (..),
                                                 checkIfAllEntitiesExist,
                                                 determineRelevantPackages,
                                                 resolveUniqueEntityIndices,
                                                 underlyingEntity)
import           Poseidon.Package               (PackageReadOptions (..),
                                                 PoseidonPackage (..),
                                                 defaultPackageReadOptions,
                                                 getJointGenotypeData,
                                                 getJointIndividualInfo,
                                                 getJointJanno,
                                                 readPoseidonPackageCollection)
import           Poseidon.Utils                 (PoseidonException (..),
                                                 PoseidonIO, envErrorLength,
                                                 envLogAction, logInfo,
                                                 logWithEnv)
import           SequenceFormats.Eigenstrat     (EigenstratSnpEntry (..),
                                                 GenoLine)
import           SequenceFormats.Utils          (Chrom)
import           System.IO                      (IOMode (..), hPutStrLn,
                                                 withFile)
import           Text.Layout.Table              (asciiRoundS, column, def,
                                                 expand, rowsG, tableString,
                                                 titlesH)
import qualified Text.Parsec                    as P
import           Text.Printf                    (printf)

-- | A datatype representing the command line options for the F-Statistics command
data FstatsOptions = FstatsOptions
    { _foBaseDirs      :: [FilePath] -- ^ the list of base directories to search for packages
    -- ^ The way the Jackknife is performed
    , _foJackknifeMode :: JackknifeMode -- ^ The way the Jackknife is performed
    -- ^ a list of chromosome names to exclude from the computation
    , _foExcludeChroms :: [Chrom] -- ^ a list of chromosome names to exclude from the computation
    -- ^ A list of F-statistics to compute
    , _foStatInput     :: [FStatInput] -- ^ A list of F-statistics to compute, entered directly or via files
    , _foMaxSnps       :: Maybe Int
    , _foNoTransitions :: Bool
    , _foTableOut      :: Maybe FilePath
    , _foBlockTableOut :: Maybe FilePath
    }

data BlockData = BlockData
    { blockStartPos  :: GenomPos
    , blockEndPos    :: GenomPos
    , blockSiteCount :: Int
    -- multiple per-block-accumulators per statistics, can be used however the specific statistic needs to.
    , blockStatVal   :: [[Double]] -- multiple per-block-accumulators per statistics, can be used however the specific statistic needs to.
    -- For example, most stats will use two numbers, one for the accumulating statistic, and one for the normalisation. Some use more, like F3
    }
    deriving (Show)

data BlockAccumulator = BlockAccumulator
    { accMaybeStartPos :: IORef (Maybe GenomPos)
    , accMaybeEndPos   :: IORef (Maybe GenomPos)
    , accCount         :: IORef Int
    -- this is the key value accumulator: for each statistic there is a list of accumulators if needed, see above.
    , accValues        :: V.Vector (VUM.IOVector Double)
    -- the outer vector is boxed and immutable, the inner vector is unboxed and mutable, so that values can be updated as we loop through the data.
    }

-- | The main function running the FStats command.
runFstats :: FstatsOptions -> PoseidonIO ()
runFstats opts = do
    -- load packages --
    allPackages <- readPoseidonPackageCollection pacReadOpts (_foBaseDirs opts)
    (groupDefs, statSpecs) <- readFstatInput (_foStatInput opts)
    unless (null groupDefs) . logInfo $ "Found group definitions: " ++ show groupDefs

    -- ------- CHECKING WHETHER ENTITIES EXIST -----------
    -- check whether all individuals that are needed for the statistics are there, including individuals needed for the adhoc-group definitions in the config file
    let newGroups = map (Group . fst) groupDefs
        collectedStats = collectStatSpecGroups statSpecs
        -- new groups can be used on the right hand side of further group definitions, that's why we explicitly exclude them here in the end of the expression
        allEntities = nub (concatMap (map underlyingEntity . snd) groupDefs ++ collectedStats) \\ newGroups
    checkIfAllEntitiesExist allEntities =<< getJointIndividualInfo allPackages

    -- annotate all individuals in all packages with the new adhoc-group definitions where necessary
    let packagesWithNewGroups = addGroupDefs groupDefs allPackages

    -- select only the packages needed for the statistics to be computed
    relevantPackageNames <- determineRelevantPackages collectedStats =<< getJointIndividualInfo packagesWithNewGroups
    let relevantPackages = filter (flip elem relevantPackageNames . posPacNameAndVersion) packagesWithNewGroups
    logInfo $ (show . length $ relevantPackages) ++ " relevant packages for chosen statistics identified:"
    mapM_ (logInfo . show . posPacNameAndVersion) relevantPackages

    logInfo "Computing stats:"
    mapM_ (logInfo . summaryPrintFstats) statSpecs
    logA <- envLogAction
    statsFold <- buildStatSpecsFold relevantPackages statSpecs
    errLength <- envErrorLength
    blocks <- liftIO $ catch (
        runSafeT $ do
            eigenstratProd <- getJointGenotypeData logA False relevantPackages Nothing
            let eigenstratProdFiltered =
                    eigenstratProd >->
                    P.filter chromFilter >->
                    capNrSnps (_foMaxSnps opts) >-> filterTransitions (_foNoTransitions opts)
                eigenstratProdInChunks = case _foJackknifeMode opts of
                    JackknifePerChromosome  -> chunkEigenstratByChromosome eigenstratProdFiltered
                    JackknifePerN chunkSize -> chunkEigenstratByNrSnps chunkSize eigenstratProdFiltered
            let summaryStatsProd = impurely foldsM statsFold eigenstratProdInChunks
            purely P.fold list (summaryStatsProd >-> printBlockInfoPipe logA)
        ) (throwM . PoseidonGenotypeExceptionForward errLength)
    let jackknifeEstimates = processBlocks statSpecs blocks
    let nrSitesList = [sum [(vals !! i) !! 1 | BlockData _ _ _ vals <- blocks] | i <- [0..(length statSpecs - 1)]]
    let hasAscertainment = or $ do
            FStatSpec _ _ maybeAsc <- statSpecs
            case maybeAsc of
                Nothing -> return False
                _       -> return True

    -- the standard output, pretty-printed to stdout
    let tableH = if hasAscertainment
                    then ["Statistic", "a", "b", "c", "d", "NrSites", "Asc (Og, Ref)", "Asc (Lo, Up)", "Estimate_Total", "Estimate_Jackknife", "StdErr_Jackknife", "Z_score_Jackknife"]
                    else ["Statistic", "a", "b", "c", "d", "NrSites", "Estimate_Total", "Estimate_Jackknife", "StdErr_Jackknife", "Z_score_Jackknife"]
    let nrCols = length tableH
    let colSpecs = replicate nrCols (column expand def def def)
        tableB = do
            (fstat, (estimateFull, estimateJN, stdErr), nrSites) <- zip3 statSpecs jackknifeEstimates nrSitesList
            let FStatSpec fType slots maybeAsc = fstat
                abcdStr = take 4 (map show slots ++ repeat "")
                (asc1, asc2) = case maybeAsc of
                    Just (AscertainmentSpec (Just og) ref lo up) -> (show (og, ref),              show (lo, up))
                    Just (AscertainmentSpec Nothing   ref lo up) -> (show ("n/a" :: String, ref), show (lo, up))
                    _ ->                                            ("n/a",                       "n/a")
            if hasAscertainment then
                return $ [show fType] ++ abcdStr ++ [show (round nrSites :: Int), asc1, asc2] ++ [printf "%.4g" estimateFull, printf "%.4g" estimateJN, printf "%.4g" stdErr, show (estimateJN / stdErr)]
            else
                return $ [show fType] ++ abcdStr ++ [show (round nrSites :: Int)] ++ [printf "%.4g" estimateFull, printf "%.4g" estimateJN, printf "%.4g" stdErr, show (estimateJN / stdErr)]
    liftIO . putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

    -- optionally output the results into a tab-separated table
    case _foTableOut opts of
        Nothing -> return ()
        Just outFn -> liftIO . withFile outFn WriteMode $
            \h -> mapM_ (hPutStrLn h . intercalate "\t") (tableH : tableB)

    -- optionally output the block data
    case _foBlockTableOut opts of
        Nothing -> return ()
        Just fn -> liftIO . withFile fn WriteMode $ \h -> do
            let headerLine = if hasAscertainment
                    then ["Statistic", "a", "b", "c", "d", "BlockNr", "StartChrom", "StartPos", "EndChrom", "EndPos", "NrSites", "Asc (Og, Ref)", "Asc (Lo, Up)", "Block_Estimate"]
                    else ["Statistic", "a", "b", "c", "d", "BlockNr", "StartChrom", "StartPos", "EndChrom", "EndPos", "NrSites", "Block_Estimate"]
            hPutStrLn h . intercalate "\t" $ headerLine
            forM_ (zip [(1 :: Int)..] blocks) $ \(i, block)  -> do
                let BlockData startPos endPos nrSites _ = block
                    blockEstimates = processBlockIndividually statSpecs block
                forM_ (zip statSpecs blockEstimates) $ \(statSpec, blockEstimate) -> do
                    let FStatSpec fType slots maybeAsc = statSpec
                        abcdStr = take 4 (map show slots ++ repeat "")
                        (asc1, asc2) = case maybeAsc of
                            Just (AscertainmentSpec (Just og) ref lo up) -> (show (og, ref),              show (lo, up))
                            Just (AscertainmentSpec Nothing   ref lo up) -> (show ("n/a" :: String, ref), show (lo, up))
                            _ ->                                            ("n/a",                       "n/a")
                        posInfo = [show (fst startPos), show (snd startPos), show (fst endPos), show (snd endPos)]
                    if hasAscertainment then
                        hPutStrLn h . intercalate "\t" $ [show fType] ++ abcdStr ++ [show i] ++ posInfo ++ [show nrSites, asc1, asc2, show blockEstimate]
                    else
                        hPutStrLn h . intercalate "\t"  $ [show fType] ++ abcdStr ++ [show i] ++ posInfo ++ [show nrSites, show blockEstimate]
  where
    chromFilter (EigenstratSnpEntry chrom _ _ _ _ _, _) = chrom `notElem` _foExcludeChroms opts
    capNrSnps Nothing  = cat
    capNrSnps (Just n) = P.take n
    chunkEigenstratByChromosome = view (groupsBy sameChrom)
    sameChrom (EigenstratSnpEntry chrom1 _ _ _ _ _, _) (EigenstratSnpEntry chrom2 _ _ _ _ _, _) =
        chrom1 == chrom2
    chunkEigenstratByNrSnps chunkSize = view (chunksOf chunkSize)
    printBlockInfoPipe logA = for cat $ \block -> do
        logWithEnv logA . logInfo $ "computing chunk range " ++ show (blockStartPos block) ++ " - " ++
            show (blockEndPos block) ++ ", size " ++ (show . blockSiteCount) block ++ " SNPs"
        yield block

summaryPrintFstats :: FStatSpec -> String
summaryPrintFstats (FStatSpec fType slots maybeAsc) =
    let ascString = case maybeAsc of
            Nothing -> ""
            Just _  -> "_ascertained"
    in  show fType ++ "("  ++ intercalate "," (map show slots) ++ ")" ++ ascString

type EntityIndicesLookup     = M.Map PoseidonEntity [Int]
type EntityAlleleCountLookup = M.Map PoseidonEntity (Int, Int)
type EntityAlleleFreqLookup  = M.Map PoseidonEntity (Maybe Double)

-- This functioin builds the central Fold that is run over each block of sites of the input data. The return is a tuple of the internal FStats datatypes and the fold.
buildStatSpecsFold :: (MonadIO m) => [PoseidonPackage] -> [FStatSpec] -> PoseidonIO (FoldM m (EigenstratSnpEntry, GenoLine) BlockData)
buildStatSpecsFold packages fStatSpecs = do
    indInfoCollection <- getJointIndividualInfo packages
    let indivNames = map indInfoName (fst indInfoCollection)
    ploidyVec <- makePloidyVec . getJointJanno $ packages
    entityIndicesLookup <- do
        let collectedSpecs = collectStatSpecGroups fStatSpecs
        entityIndices <- sequence [resolveUniqueEntityIndices True [s] indInfoCollection | s <- collectedSpecs]
        return . M.fromList . zip collectedSpecs $ entityIndices
    blockAccum <- do
        listOfInnerVectors <- forM fStatSpecs $ \(FStatSpec fType _ _) -> do
            case fType of
                F3star     -> liftIO $ VUM.replicate 4 0.0 -- F3star has four accumulators: one numerator, one denominator, and one normaliser for each of the two.
                FSTvanilla -> liftIO $ VUM.replicate 4 0.0 -- same as with F3star
                FST        -> liftIO $ VUM.replicate 4 0.0 -- same as with F3star
                _          -> liftIO $ VUM.replicate 2 0.0 -- all other statistics have just one value and one normaliser.
        liftIO $ BlockAccumulator <$> newIORef Nothing <*> newIORef Nothing <*> newIORef 0 <*> pure (V.fromList listOfInnerVectors)
    return $ FoldM (step ploidyVec indivNames entityIndicesLookup blockAccum) (initialize blockAccum) (extract blockAccum)
  where
    step :: (MonadIO m) => PloidyVec -> [String] -> EntityIndicesLookup -> BlockAccumulator -> () -> (EigenstratSnpEntry, GenoLine) -> m ()
    step ploidyVec indivNames entIndLookup blockAccum _ (EigenstratSnpEntry c p _ _ _ _, genoLine) = do
        -- this function is called for every SNP.
        startPos <- liftIO $ readIORef (accMaybeStartPos blockAccum)
        case startPos of
            Nothing -> liftIO $ writeIORef (accMaybeStartPos blockAccum) (Just (c, p))
            Just _  -> return ()
        liftIO $ writeIORef (accMaybeEndPos blockAccum) (Just (c, p))
        let alleleCountLookupF = M.map (computeAlleleCount genoLine ploidyVec indivNames) entIndLookup
            alleleFreqLookupF  = M.map (computeAlleleFreq  genoLine ploidyVec indivNames) entIndLookup
        forM_ (zip [0..] fStatSpecs) $ \(i, fStatSpec) -> do
            checkForIllegalSampleSizes ploidyVec entIndLookup fStatSpec
            -- loop over all statistics
            let maybeAccValues = computeFStatAccumulators fStatSpec alleleCountLookupF alleleFreqLookupF  -- compute the accumulating values for that site.
            forM_ (zip [0..] maybeAccValues) $ \(j, x) ->
                liftIO $ VUM.modify (accValues blockAccum V.! i) (+x) j -- add the value to the respective accumulator.
        -- counts the number of SNPs in a block, and ignores missing data. Missing data is considered within each accumulator.
        liftIO $ modifyIORef' (accCount blockAccum) (+1)
        return ()
    initialize :: (MonadIO m) => BlockAccumulator -> m ()
    initialize (BlockAccumulator startRef endRef countRef valVec) = do
        liftIO $ writeIORef startRef Nothing
        liftIO $ writeIORef endRef Nothing
        liftIO $ writeIORef countRef 0
        forM_ (V.toList valVec) $ \vals -> do
            liftIO $ VUM.set vals 0.0
    extract :: (MonadIO m) => BlockAccumulator -> () -> m BlockData
    extract (BlockAccumulator maybeStartPosRef maybeEndPosRef countRef valVec) _ = do
        maybeStartPos <- liftIO $ readIORef maybeStartPosRef
        maybeEndPos <- liftIO $ readIORef maybeEndPosRef
        count <- liftIO $ readIORef countRef
        statVals <- liftIO $ mapM (fmap VU.toList . VU.freeze) (V.toList valVec) -- this unfolds the vector of mutable vectors into a list of lists.
        case (maybeStartPos, maybeEndPos) of
            (Just startPos, Just endPos) -> return $ BlockData startPos endPos count statVals
            _ -> error "should never happen"

computeFStatAccumulators :: FStatSpec -> EntityAlleleCountLookup -> EntityAlleleFreqLookup -> [Double] -- returns a number of accumulated variables, in most cases a value and a normalising count,
-- but in case of F3, for example, also a second accumulator and its normaliser for capturing the heterozygosity
computeFStatAccumulators (FStatSpec fType slots maybeAsc) alleleCountF alleleFreqF =
    let caf = (alleleFreqF M.!) -- this returns Nothing if missing data
        cac e = case alleleCountF M.! e of -- this also returns Nothing if missing data.
            (_, 0) -> Nothing
            x      -> Just x
        ascCond = fromMaybe False $ case maybeAsc of
            Nothing -> return True
            Just (AscertainmentSpec maybeOg ascRef lo hi) -> do -- Maybe Monad - if any of the required allele frequencies are missing, this returns a Nothing
                ascFreq <- case maybeOg of
                    Nothing -> do
                        x <- caf ascRef
                        if x > 0.5 then return (1.0 - x) else return x -- use minor allele frequency if no outgroup is given
                    Just og -> do -- Maybe Monad
                        (ogNonRef, ogNonMiss) <- cac og
                        x <- caf ascRef
                        if ogNonRef == 0 then return x else
                            if ogNonRef == ogNonMiss then return (1.0 - x) else Nothing
                return $ ascFreq >= lo && ascFreq <= hi
    in  if ascCond then
            case (fType, slots) of
                (F2vanilla, [a, b]) -> if a == b then [0.0, 1.0] else
                    retWithNormAcc $ computeF2vanilla <$> caf a <*> caf b
                (F2, [a, b]) -> if a == b then [0.0, 1.0] else
                    retWithNormAcc $ computeF2 <$> cac a <*> cac b
                (F3vanilla, [a, b, c]) -> if c == a || c == b then [0.0, 1.0] else
                    retWithNormAcc $ computeF3vanilla <$> caf a <*> caf b <*> caf c
                (F3, [a, b, c]) -> if c == a || c == b then [0.0, 1.0]
                    else retWithNormAcc $ computeF3 <$> caf a <*> caf b <*> cac c
                (F3star, [a, b, c]) -> if c == a || c == b then [0.0, 1.0, 1.0, 1.0] else
                    retWithNormAcc (computeF3 <$> caf a <*> caf b <*> cac c) ++
                    retWithNormAcc (computeHet <$> cac c)
                (F4, [a, b, c, d]) -> if a == b || c == d then [0.0, 1.0] else
                    retWithNormAcc $ computeF4 <$> caf a <*> caf b <*> caf c <*> caf d
                (PWM, [a, b]) -> retWithNormAcc $ computePWM <$> caf a <*> caf b
                (Het, [a]) -> retWithNormAcc $ computeHet <$> cac a
                (FSTvanilla, [a, b]) -> if a == b then [0.0, 1.0, 1.0, 1.0] else
                    retWithNormAcc (computeF2vanilla <$> caf a <*> caf b) ++
                    retWithNormAcc (computePWM <$> caf a <*> caf b)
                (FST, [a, b]) -> if a == b then [0.0, 1.0, 1.0, 1.0] else
                    retWithNormAcc (computeF2 <$> cac a <*> cac b) ++
                    retWithNormAcc (computePWM <$> caf a <*> caf b)
                _ -> error "should never happen"
        else
            case fType of
                F3         -> [0.0, 0.0, 0.0, 0.0]
                FSTvanilla -> [0.0, 0.0, 0.0, 0.0]
                FST        -> [0.0, 0.0, 0.0, 0.0]
                _          -> [0.0, 0.0]
  where
    retWithNormAcc (Just x) = [x, 1.0]
    retWithNormAcc Nothing  = [0.0, 0.0]
    -- these formulas are mostly taken from Patterson et al. 2012 Appendix A (page 25 in the PDF), and FST from Bhatia et al. 2013.
    computeF4         a b c d = (a - b) * (c - d)
    computeF3vanilla  a b c   = (c - a) * (c - b)
    computeF2vanilla  a b     = (a - b) * (a - b)
    computePWM        a b     = a * (1.0 - b) + (1.0 - a) * b
    computeHet (na, sa) = 2.0 * fromIntegral (na * (sa - na)) / fromIntegral (sa * (sa - 1))
    computeF3 a b (nc, sc) =
        let c = computeFreq nc sc
            corrFac = 0.5 * computeHet (nc, sc) / fromIntegral sc
        in  computeF3vanilla a b c - corrFac
    computeF2 (na, sa) (nb, sb) =
        let a = computeFreq na sa
            b = computeFreq nb sb
            corrFac = 0.5 * computeHet (na, sa) / fromIntegral sa + 0.5 * computeHet (nb, sb) / fromIntegral sb
        in  computeF2vanilla a b - corrFac
    computeFreq na sa = fromIntegral na / fromIntegral sa

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

collectStatSpecGroups :: [FStatSpec] -> [PoseidonEntity]
collectStatSpecGroups statSpecs = nub $ do
    FStatSpec _ slots maybeAsc <- statSpecs
    case maybeAsc of
        Just (AscertainmentSpec (Just og) ref _ _) -> slots ++ [og, ref]
        Just (AscertainmentSpec Nothing ref _ _)   -> slots ++ [ref]
        Nothing                                    -> slots

-- returns the total estimate, the jackknife estimate, and the jackknife error
processBlocks :: [FStatSpec] -> [BlockData] -> [(Double, Double, Double)]
processBlocks statSpecs blocks = do
    let block_weights = map (fromIntegral . blockSiteCount) blocks
    (i, FStatSpec fType _ _ ) <- zip [0..] statSpecs
    if isRatio fType then
            let numerator_values = map ((!!0) . (!!i) . blockStatVal) blocks
                numerator_norm = map ((!!1) . (!!i) . blockStatVal) blocks
                denominator_values = map ((!!2) . (!!i) . blockStatVal) blocks
                denominator_norm = map ((!!3) . (!!i) . blockStatVal) blocks
                num_full = sum numerator_values / sum numerator_norm
                denom_full = sum denominator_values / sum denominator_norm
                full_estimate = num_full / denom_full
                partial_estimates = do
                    j <- [0..(length block_weights - 1)]
                    let num = sum [v | (k, v) <- zip [0..] numerator_values, k /= j]
                        num_norm = sum [v | (k, v) <- zip [0..] numerator_norm, k /= j]
                        denom = sum [v | (k, v) <- zip [0..] denominator_values, k /= j]
                        denom_norm = sum [v | (k, v) <- zip [0..] denominator_norm, k /= j]
                    return $ (num / num_norm) / (denom / denom_norm)
                (estimateJackknife, stdErrJackknife) = computeJackknifeOriginal full_estimate block_weights partial_estimates
            in  return (full_estimate, estimateJackknife, stdErrJackknife)
        else
            let values = map ((!!0) . (!!i) . blockStatVal) blocks
                norm = map ((!!1) . (!!i) . blockStatVal) blocks
                full_estimate = sum values / sum norm
                partial_estimates = do
                    j <- [0..(length block_weights - 1)]
                    let val' = sum [v | (k, v) <- zip [0..] values, k /= j]
                        norm' = sum [v | (k, v) <- zip [0..] norm, k /= j]
                    return $ val' / norm'
                (estimateJackknife, stdErrJackknife) = computeJackknifeOriginal full_estimate block_weights partial_estimates
            in  return (full_estimate, estimateJackknife, stdErrJackknife)

isRatio :: FStatType -> Bool
isRatio F3star     = True
isRatio FSTvanilla = True
isRatio FST        = True
isRatio _          = False

-- outer list: stats, inner list: estimates per block
processBlockIndividually :: [FStatSpec] -> BlockData -> [Double]
processBlockIndividually statSpecs (BlockData _ _ _ allStatVals) = do
    (FStatSpec fType _ _, statVals) <- zip statSpecs allStatVals
    if isRatio fType then
            let numerator_value = statVals !! 0
                numerator_norm = statVals !! 1
                denominator_value = statVals !! 2
                denominator_norm = statVals !! 3
                num_full = numerator_value / numerator_norm
                denom_full = denominator_value / denominator_norm
            in  return $ num_full / denom_full
        else
            let value = statVals !! 0
                norm = statVals !! 1
            in  return $ value / norm

checkForIllegalSampleSizes :: (MonadIO m) => PloidyVec -> EntityIndicesLookup -> FStatSpec -> m ()
checkForIllegalSampleSizes ploidyVec entIndLookup (FStatSpec fType slots _) = do
    case fType of
        F2 -> do
            let nA = getNrHaplotypes (slots !! 0)
            let nB = getNrHaplotypes (slots !! 1)
            when (nA < 2 || nB < 2) $
                liftIO . throwIO $ FStatException "Unbiased F2(A, B) estimators need both A and B \
                        \to have more than one haplotype. You might want to try F2vanilla"
        FST -> do
            let nA = getNrHaplotypes (slots !! 0)
            let nB = getNrHaplotypes (slots !! 1)
            when (nA < 2 || nB < 2) $
                liftIO . throwIO $ FStatException "Unbiased FST(A, B) estimators need both A and B \
                        \to have more than one haplotype. You might want to try FSTvanilla"
        F3 -> do
            let nC = getNrHaplotypes (slots !! 2)
            when (nC < 2) $
                liftIO . throwIO $ FStatException "Unbiased F3(A, B; C) estimators need C \
                        \to have more than one haplotype. You might want to try F3vanilla"
        F3star -> do
            let nC = getNrHaplotypes (slots !! 2)
            when (nC < 2) $
                liftIO . throwIO $ FStatException "Unbiased F3star(A, B; C) estimators need C \
                        \to have more than one haplotype. You might want to try F3vanilla"
        Het -> do
            let nA = getNrHaplotypes (slots !! 0)
            when (nA < 2) $
                liftIO . throwIO $ FStatException "Het(A) requires A \
                        \to have more than one haplotype"
        _  -> return ()
  where
    getNrHaplotypes :: PoseidonEntity -> Int
    getNrHaplotypes entity =
        let inds = entIndLookup M.! entity
        in  sum . map (haps . (ploidyVec V.!)) $ inds
    haps Haploid = 1
    haps Diploid = 2
