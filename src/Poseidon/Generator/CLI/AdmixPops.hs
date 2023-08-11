module Poseidon.Generator.CLI.AdmixPops where

import           Poseidon.Generator.Parsers
import           Poseidon.Generator.SampleGeno
import           Poseidon.Generator.Types
import           Poseidon.Generator.Utils

import           Control.Exception             (catch, throwIO)
import           Control.Monad                 (forM, unless, when)
import           Data.Function                 ((&))
import           Data.List
import           Data.Maybe
import           Data.Time                     (getCurrentTime)
import           Lens.Family2                  (view)
import           Pipes
import qualified Pipes.Group                   as PG
import qualified Pipes.Prelude                 as P
import           Pipes.Safe                    (runSafeT)
import           Poseidon.GenotypeData
import           Poseidon.Janno
import           Poseidon.Package
import           Poseidon.Utils
import           SequenceFormats.Eigenstrat
import           SequenceFormats.Plink         (eigenstratInd2PlinkFam,
                                                writePlink)
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath               (takeBaseName, (<.>), (</>))

data AdmixPopsMethodSettings =
    PerSNP {
        _admixMarginalizeMissing :: Bool
    } |
    InChunks {
        _admixChunkSize :: Int
    }

data AdmixPopsOptions = AdmixPopsOptions {
      _admixGenoSources             :: [GenoDataSource]
    , _admixIndWithAdmixtureSet     :: [RequestedInd]
    , _admixIndWithAdmixtureSetFile :: Maybe FilePath
    , _admixMethodSettings          :: AdmixPopsMethodSettings
    , _admixOutFormat               :: GenotypeFormatSpec
    , _admixOutPath                 :: FilePath
    , _forgeOutPacName              :: Maybe String
    , _forgeOutputPlinkPopMode      :: PlinkPopNameMode
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

runAdmixPops :: AdmixPopsOptions -> PoseidonIO ()
runAdmixPops (
    AdmixPopsOptions
        genoSources
        popsWithFracsDirect
        popsWithFracsFile
        methodSetting
        outFormat
        outPath
        maybeOutName
        outPlinkPopMode
    ) = do
    -- compile individuals
    popsWithFracsFromFile <- case popsWithFracsFile of
        Nothing -> return []
        Just f  -> liftIO $ readIndWithAdmixtureSetFromFile f
    let requestedInds = popsWithFracsDirect ++ popsWithFracsFromFile
    -- validating input
    logInfo "Checking requested, artificial individuals"
    logInfo $ "Individuals: " ++ renderRequestedInds requestedInds
    liftIO $ checkIndsWithAdmixtureSets requestedInds
    -- load Poseidon packages
    properPackages <- readPoseidonPackageCollection pacReadOpts $ [getPacBaseDirs x | x@PacBaseDir {} <- genoSources]
    pseudoPackages <- mapM makePseudoPackageFromGenotypeData $ [getGenoDirect x | x@GenoDirect {} <- genoSources]
    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    let allPackages = properPackages ++ pseudoPackages
    -- determine relevant packages
    let popNames = concat $ map (map _inPopName) $ map _inPopSet requestedInds
    relevantPackages <- filterPackagesByPops popNames allPackages
    -- gather additional info for requested inds
    preparedInds <- mapM (`gatherInfoForInd` relevantPackages) requestedInds
    -- create new package --
    let outName = case maybeOutName of -- take basename of outPath, if name is not provided
            Just x  -> x
            Nothing -> takeBaseName outPath
    when (outName == "") $ liftIO $ throwIO PoseidonEmptyOutPacNameException
    -- create new directory
    logInfo $ "Writing to directory (will be created if missing): " ++ outPath
    liftIO $ createDirectoryIfMissing True outPath
    -- compile genotype data structure
    let (outInd, outSnp, outGeno) = case outFormat of
            GenotypeFormatEigenstrat -> (outName <.> ".ind", outName <.> ".snp", outName <.> ".geno")
            GenotypeFormatPlink -> (outName <.> ".fam", outName <.> ".bim", outName <.> ".bed")
    let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing Nothing
        pac = newMinimalPackageTemplate outPath outName genotypeData
    liftIO $ writePoseidonPackage pac
    -- compile genotype data
    logInfo "Compiling individuals"
    logA <- envLogAction
    currentTime <- liftIO getCurrentTime
    liftIO $ catch (
        runSafeT $ do
            (_, eigenstratProd) <- getJointGenotypeData logA False outPlinkPopMode relevantPackages Nothing
            let (outG, outS, outI) = (outPath </> outGeno, outPath </> outSnp, outPath </> outInd)
            let newIndEntries = map (\x -> EigenstratIndEntry (_indName x) Unknown (_groupName x)) preparedInds
            let outConsumer = case outFormat of
                    GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI newIndEntries
                    GenotypeFormatPlink      -> writePlink      outG outS outI (map (eigenstratInd2PlinkFam outPlinkPopMode) newIndEntries)
            case methodSetting of
                PerSNP marginalizeMissing -> do
                    runEffect $ eigenstratProd >->
                        printSNPCopyProgress logA currentTime >->
                        P.mapM (samplePerSNP marginalizeMissing preparedInds) >->
                        outConsumer
                InChunks chunkSize -> do
                    runEffect $ (
                            eigenstratProd &
                            chunkEigenstratByNrSnps chunkSize &
                            PG.maps (samplePerChunk logA preparedInds) &
                            PG.concats
                        ) >->
                        printSNPCopyProgress logA currentTime >->
                        outConsumer
        ) (\e -> throwIO $ PoseidonGenotypeExceptionForward e)
    logInfo "Done"
    where
        chunkEigenstratByNrSnps chunkSize = view (PG.chunksOf chunkSize)

checkIndsWithAdmixtureSets :: [RequestedInd] -> IO ()
checkIndsWithAdmixtureSets requestedInds = do
    checkDuplicateIndNames requestedInds
    mapM_ checkPopFracList requestedInds
    where
        checkDuplicateIndNames :: [RequestedInd] -> IO ()
        checkDuplicateIndNames xs =
            let individualsGrouped = filter (\x -> length x > 1) $ group $ sort $ map _inIndName xs
            in unless (null individualsGrouped) $ do
                throwIO $ PoseidonGeneratorCLIParsingException $
                    "Duplicate individual names: " ++ intercalate "," (nub $ concat individualsGrouped)
        checkPopFracList :: RequestedInd -> IO ()
        checkPopFracList x = do
            let xs = _inPopSet x
                fracs = map _inPopFrac xs
            when (sum fracs /= 1) $ do
                throwIO $ PoseidonGeneratorCLIParsingException $
                    "Fractions in " ++ show x ++ " do not to sum to 100%"

filterPackagesByPops :: [String] -> [PoseidonPackage] -> PoseidonIO [PoseidonPackage]
filterPackagesByPops pops packages = do
    fmap catMaybes . forM packages $ \pac -> do
        inds <- loadIndividuals (posPacBaseDir pac) (posPacGenotypeData pac)
        let groupNamesPac = [groupName | EigenstratIndEntry _ _ groupName <- inds]
        if   not (null (groupNamesPac `intersect` pops))
        then return (Just pac)
        else return Nothing

gatherInfoForInd :: RequestedInd -> [PoseidonPackage] -> PoseidonIO IndConcrete
gatherInfoForInd (RequestedInd name_ group_ set_) pacs = do
    inds <- mapM (`extractIndsPerPop` pacs) set_
    return $ IndConcrete name_ group_ inds

extractIndsPerPop :: PopFrac -> [PoseidonPackage] -> PoseidonIO PopFracConcrete
extractIndsPerPop (PopFrac pop_ frac_) relevantPackages = do
    let allPackageNames = map posPacTitle relevantPackages
    allIndEntries <- mapM (\pac -> loadIndividuals (posPacBaseDir pac) (posPacGenotypeData pac)) relevantPackages
    let filterFunc (_,_,EigenstratIndEntry _ _ _group) = _group == pop_
        indNames = map extractIndName $ filter filterFunc (zipGroup allPackageNames allIndEntries)
        indIDs = map extractFirst $ filter filterFunc (zipGroup allPackageNames allIndEntries)
    return (PopFracConcrete pop_ frac_ (zip indNames indIDs))
    where
        extractIndName (_,_,EigenstratIndEntry x _ _) = x
