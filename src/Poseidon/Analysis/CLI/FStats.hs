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
                                                 addGroupDefs,
                                                 computeAlleleCount,
                                                 computeAlleleFreq,
                                                 computeJackknifeOriginal,
                                                 filterTransitions,
                                                 resolveEntityIndicesIO)

import           Control.Exception              (catch, throwIO)
import           Control.Foldl                  (FoldM (..), impurely, list,
                                                 purely)
import           Control.Monad                  (forM, forM_, unless)
import           Control.Monad.IO.Class         (MonadIO, liftIO)
import           Data.IORef                     (IORef, modifyIORef', newIORef,
                                                 readIORef, writeIORef)
import           Data.List                      (intercalate, nub, (\\))
import qualified Data.Map                       as M
import qualified Data.Vector                    as V
import qualified Data.Vector.Unboxed            as VU
import qualified Data.Vector.Unboxed.Mutable    as VUM
-- import           Debug.Trace                 (trace)
import           Lens.Family2                   (view)
import           Pipes                          (cat, for, yield, (>->))
import           Pipes.Group                    (chunksOf, foldsM, groupsBy)
import qualified Pipes.Prelude                  as P
import           Pipes.Safe                     (runSafeT)
import           Poseidon.EntitiesList          (PoseidonEntity (..),
                                                 findNonExistentEntities,
                                                 indInfoFindRelevantPackageNames,
                                                 underlyingEntity)
import           Poseidon.Package               (PackageReadOptions (..),
                                                 PoseidonPackage (..),
                                                 defaultPackageReadOptions,
                                                 getJointGenotypeData,
                                                 getJointIndividualInfo,
                                                 readPoseidonPackageCollection)
import           Poseidon.SecondaryTypes        (IndividualInfo (..))
import           Poseidon.Utils                 (PoseidonException (..),
                                                 PoseidonIO, envInputPlinkMode,
                                                 envLogAction, logError,
                                                 logInfo, logWithEnv)
import           SequenceFormats.Eigenstrat     (EigenstratSnpEntry (..),
                                                 GenoLine)
import           SequenceFormats.Utils          (Chrom)
import           System.Exit                    (exitFailure)
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

    -- check whether all individuals that are needed for the statistics are there, including individuals needed for the adhoc-group definitions in the config file
    let newGroups = map (Group . fst) groupDefs
    let collectedStats = collectStatSpecGroups statSpecs
    let allEntities = nub (concatMap (map underlyingEntity . snd) groupDefs ++ collectedStats) \\ newGroups

    let jointIndInfoAll = getJointIndividualInfo allPackages
    let missingEntities = findNonExistentEntities allEntities jointIndInfoAll

    if not. null $ missingEntities then do
        logError $ "The following entities couldn't be found: " ++ (intercalate ", " . map show $ missingEntities)
        liftIO exitFailure
    else do

        -- annotate all individuals with the new adhoc-group definitions where necessary
        let jointIndInfoWithNewGroups = addGroupDefs groupDefs jointIndInfoAll

        -- select only the packages needed for the statistics to be computed
        let relevantPackageNames = indInfoFindRelevantPackageNames collectedStats jointIndInfoWithNewGroups
        let relevantPackages = filter (flip elem relevantPackageNames . posPacTitle) allPackages
        logInfo $ (show . length $ relevantPackages) ++ " relevant packages for chosen statistics identified:"
        mapM_ (logInfo . posPacTitle) relevantPackages

        -- annotate again the individuals in the selected packages with the adhoc-group defs from the config
        let jointIndInfo = addGroupDefs groupDefs . getJointIndividualInfo $ relevantPackages

        logInfo "Computing stats:"
        mapM_ (logInfo . summaryPrintFstats) statSpecs
        logA <- envLogAction
        inPlinkPopMode <- envInputPlinkMode
        statsFold <- buildStatSpecsFold jointIndInfo statSpecs
        blocks <- liftIO $ catch (
            runSafeT $ do
                (_, eigenstratProd) <- getJointGenotypeData logA False inPlinkPopMode relevantPackages Nothing
                let eigenstratProdFiltered =
                        eigenstratProd >->
                        P.filter chromFilter >->
                        capNrSnps (_foMaxSnps opts) >-> filterTransitions (_foNoTransitions opts)
                    eigenstratProdInChunks = case _foJackknifeMode opts of
                        JackknifePerChromosome  -> chunkEigenstratByChromosome eigenstratProdFiltered
                        JackknifePerN chunkSize -> chunkEigenstratByNrSnps chunkSize eigenstratProdFiltered
                let summaryStatsProd = impurely foldsM statsFold eigenstratProdInChunks
                purely P.fold list (summaryStatsProd >-> printBlockInfoPipe logA)
            ) (throwIO . PoseidonGenotypeExceptionForward)
        let jackknifeEstimates = processBlocks statSpecs blocks
        let nrSitesList = [sum [(vals !! i) !! 1 | BlockData _ _ _ vals <- blocks] | i <- [0..(length statSpecs - 1)]]
        let hasAscertainment = or $ do
                FStatSpec _ _ maybeAsc <- statSpecs
                case maybeAsc of
                    Nothing -> return False
                    _       -> return True

        -- the standard output, pretty-printed to stdout
        let nrCols = if hasAscertainment then 11 else 9
        let colSpecs = replicate nrCols (column expand def def def)
            tableH = if hasAscertainment
                     then ["Statistic", "a", "b", "c", "d", "NrSites", "Asc (Og, Ref)", "Asc (Lo, Up)", "Estimate", "StdErr", "Z score"]
                     else ["Statistic", "a", "b", "c", "d", "NrSites", "Estimate", "StdErr", "Z score"]
            tableB = do
                (fstat, (estimate, stdErr), nrSites) <- zip3 statSpecs jackknifeEstimates nrSitesList
                let FStatSpec fType slots maybeAsc = fstat
                    abcdStr = take 4 (map show slots ++ repeat "")
                    (asc1, asc2) = case maybeAsc of
                        Just (AscertainmentSpec (Just og) ref lo up) -> (show (og, ref),              show (lo, up))
                        Just (AscertainmentSpec Nothing   ref lo up) -> (show ("n/a" :: String, ref), show (lo, up))
                        _ ->                                            ("n/a",                       "n/a")
                if hasAscertainment then
                    return $ [show fType] ++ abcdStr ++ [show (round nrSites :: Int), asc1, asc2] ++ [printf "%.4g" estimate, printf "%.4g" stdErr, show (estimate / stdErr)]
                else
                    return $ [show fType] ++ abcdStr ++ [show (round nrSites :: Int)] ++ [printf "%.4g" estimate, printf "%.4g" stdErr, show (estimate / stdErr)]
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
buildStatSpecsFold :: (MonadIO m) => [IndividualInfo] -> [FStatSpec] -> PoseidonIO (FoldM m (EigenstratSnpEntry, GenoLine) BlockData)
buildStatSpecsFold indInfo fStatSpecs = do
    entityIndicesLookup <- do
        let collectedSpecs = collectStatSpecGroups fStatSpecs
        entityIndices <- sequence [resolveEntityIndicesIO [s] indInfo | s <- collectedSpecs]
        return . M.fromList . zip collectedSpecs $ entityIndices
    blockAccum <- do
        listOfInnerVectors <- forM fStatSpecs $ \(FStatSpec fType _ _) -> do
            case fType of
                F3 -> liftIO $ VUM.replicate 4 0.0 --only F3 has four accumulators: one numerator, one denominator, and one normaliser for each of the two.
                _  -> liftIO $ VUM.replicate 2 0.0 -- all other statistics have just one value and one normaliser.
        liftIO $ BlockAccumulator <$> newIORef Nothing <*> newIORef Nothing <*> newIORef 0 <*> pure (V.fromList listOfInnerVectors)
    return $ FoldM (step entityIndicesLookup blockAccum) (initialize blockAccum) (extract blockAccum)
  where
    step :: (MonadIO m) => EntityIndicesLookup -> BlockAccumulator -> () -> (EigenstratSnpEntry, GenoLine) -> m ()
    step entIndLookup blockAccum _ (EigenstratSnpEntry c p _ _ _ _, genoLine) = do
        -- this function is called for every SNP.
        startPos <- liftIO $ readIORef (accMaybeStartPos blockAccum)
        case startPos of
            Nothing -> liftIO $ writeIORef (accMaybeStartPos blockAccum) (Just (c, p))
            Just _  -> return ()
        liftIO $ writeIORef (accMaybeEndPos blockAccum) (Just (c, p))
        let alleleCountLookupF = M.map (computeAlleleCount genoLine) entIndLookup
            alleleFreqLookupF  = M.map (computeAlleleFreq  genoLine) entIndLookup
        forM_ (zip [0..] fStatSpecs) $ \(i, fStatSpec) -> do
            -- loop over all statistics
            let maybeAccValues = computeFStatAccumulators fStatSpec alleleCountLookupF alleleFreqLookupF  -- compute the accumulating values for that site.
            forM_ (zip [0..] maybeAccValues) $ \(j, maybeAccVal) -> do
                case maybeAccVal of
                    Nothing -> return ()
                    Just x -> liftIO $ VUM.modify (accValues blockAccum V.! i) (+x) j -- add the value to the respective accumulator.
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

computeFStatAccumulators :: FStatSpec -> EntityAlleleCountLookup -> EntityAlleleFreqLookup -> [Maybe Double] -- returns a number of accumulated variables, in most cases a value and a normalising count,
-- but in case of F3, for example, also a second accumulator and its normaliser for capturing the heterozygosity
computeFStatAccumulators (FStatSpec fType slots maybeAsc) alleleCountF alleleFreqF =
    let caf = (alleleFreqF M.!) -- this returns Nothing if missing data
        cac e = case alleleCountF M.! e of -- this also returns Nothing if missing data.
            (_, 0) -> Nothing
            x      -> Just x
        ascCond = case maybeAsc of
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
        applyAsc x = do -- Maybe Monad
            cond <- ascCond
            if cond then return x else return 0.0 -- set statistic result to 0.0 if ascertainment is False
    in  case (fType, slots) of
            (F4,         [a, b, c, d]) -> retWithNormAcc $ (computeF4         <$> caf a <*> caf b <*> caf c <*> caf d) >>= applyAsc
            (F3vanilla,  [a, b, c])    -> retWithNormAcc $ (computeF3vanilla  <$> caf a <*> caf b <*> caf c)           >>= applyAsc
            (F2vanilla,  [a, b])       -> retWithNormAcc $ (computeF2vanilla  <$> caf a <*> caf b)                     >>= applyAsc
            (PWM,        [a, b])       -> retWithNormAcc $ (computePWM        <$> caf a <*> caf b)                     >>= applyAsc
            (Het,        [a])          -> retWithNormAcc $ (computeHet        <$> cac a)                               >>= applyAsc
            (F2,         [a, b])       -> retWithNormAcc $ (computeF2         <$> cac a <*> cac b)                     >>= applyAsc
            (FSTvanilla, [a, b])       -> retWithNormAcc $ (computeFSTvanilla    (caf a)   (caf b))                    >>= applyAsc
            (FST,        [a, b])       -> retWithNormAcc $ (computeFST           (cac a)   (cac b))                    >>= applyAsc
            (F3,         [a, b, c])    ->
                retWithNormAcc ((computeF3noNorm <$> caf a <*> caf b <*> cac c) >>= applyAsc) ++
                retWithNormAcc ((computeHet <$> cac c) >>= applyAsc)
            _ -> error "should never happen"
  where
    retWithNormAcc (Just x) = [Just x, Just 1.0]
    retWithNormAcc Nothing  = [Nothing, Nothing]
    -- these formulas are mostly taken from Patterson et al. 2012 Appendix A (page 25 in the PDF)
    computeF4         a b c d = (a - b) * (c - d)
    computeF3vanilla  a b c   = (c - a) * (c - b)
    computeF2vanilla  a b     = (a - b) * (a - b)
    computePWM        a b     = a * (1.0 - b) + (1.0 - a) * b
    computeHet (na, sa) = 2.0 * fromIntegral (na * (sa - na)) / fromIntegral (sa * (sa - 1))
    computeF3noNorm a b (nc, sc) =
        let c = computeFreq nc sc
            corrFac = 0.5 * computeHet (nc, sc) / fromIntegral sc
        in  computeF3vanilla a b c - corrFac
    computeF2 (na, sa) (nb, sb) =
        let a = computeFreq na sa
            b = computeFreq nb sb
            corrFac = 0.5 * computeHet (na, sa) / fromIntegral sa + 0.5 * computeHet (nb, sb) / fromIntegral sb
        in  computeF2vanilla a b - corrFac
    computeFSTvanilla maybeA maybeB =
        case (maybeA, maybeB) of
            (Just a, Just b) ->
                let num = (a - b)^(2 :: Int)
                    denom = a * (1 - b) + b * (1 - a)
                in  if denom > 0 then Just (num / denom) else Nothing
            _ -> Nothing
    computeFST maybeNaSa maybeNbSb = case (maybeNaSa, maybeNbSb) of
        (Just (na, sa), Just (nb, sb)) ->
            let num = computeF2 (na, sa) (nb, sb)
                denom = computeF2 (na, sa) (nb, sb) + 0.5 * computeHet (na, sa) + 0.5 * computeHet (nb, sb)
            in  if denom > 0 then Just (num / denom) else Nothing
        _ -> Nothing
    computeFreq na sa = fromIntegral na / fromIntegral sa

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = True
    , _readOptIgnoreChecksums  = True
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

processBlocks :: [FStatSpec] -> [BlockData] -> [(Double, Double)]
processBlocks statSpecs blocks = do
    let block_weights = map (fromIntegral . blockSiteCount) blocks
    (i, FStatSpec fType _ _ ) <- zip [0..] statSpecs
    case fType of
        F3 ->
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
            in  return $ computeJackknifeOriginal full_estimate block_weights partial_estimates
        _ ->
            let values = map ((!!0) . (!!i) . blockStatVal) blocks
                norm = map ((!!1) . (!!i) . blockStatVal) blocks
                full_estimate = sum values / sum norm
                partial_estimates = do
                    j <- [0..(length block_weights - 1)]
                    let val' = sum [v | (k, v) <- zip [0..] values, k /= j]
                        norm' = sum [v | (k, v) <- zip [0..] norm, k /= j]
                    return $ val' / norm'
            in  return $ computeJackknifeOriginal full_estimate block_weights partial_estimates

-- outer list: stats, inner list: estimates per block
processBlockIndividually :: [FStatSpec] -> BlockData -> [Double]
processBlockIndividually statSpecs (BlockData _ _ _ allStatVals) = do
    (FStatSpec fType _ _, statVals) <- zip statSpecs allStatVals
    case fType of
        F3 ->
            let numerator_value = statVals !! 0
                numerator_norm = statVals !! 1
                denominator_value = statVals !! 2
                denominator_norm = statVals !! 3
                num_full = numerator_value / numerator_norm
                denom_full = denominator_value / denominator_norm
            in  return $ num_full / denom_full
        _ ->
            let value = statVals !! 0
                norm = statVals !! 1
            in  return $ value / norm
