{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Forge where

import           Poseidon.BibFile            (BibEntry (..), BibTeX,
                                              writeBibTeXFile)
import           Poseidon.EntitiesList       (EntityInput, PoseidonEntity (..),
                                              PoseidonIndividual (..),
                                              SignedEntity (..),
                                              filterRelevantPackages,
                                              findNonExistentEntities,
                                              readEntityInputs,
                                              resolveEntityIndices)
import           Poseidon.GenotypeData       (GenoDataSource (..),
                                              GenotypeDataSpec (..),
                                              GenotypeFormatSpec (..),
                                              SNPSetSpec (..),
                                              printSNPCopyProgress,
                                              selectIndices, snpSetMergeList)
import           Poseidon.Janno              (JannoList (..), JannoRow (..),
                                              writeJannoFile)
import           Poseidon.Package            (PackageReadOptions (..),
                                              PoseidonPackage (..),
                                              defaultPackageReadOptions,
                                              getJointGenotypeData,
                                              getJointIndividualInfo,
                                              getJointJanno,
                                              makePseudoPackageFromGenotypeData,
                                              newMinimalPackageTemplate,
                                              newPackageTemplate,
                                              readPoseidonPackageCollection,
                                              writePoseidonPackage)
import           Poseidon.SecondaryTypes     (IndividualInfo (..))
import           Poseidon.Utils              (PoseidonException (..),
                                              PoseidonLogIO,
                                              determinePackageOutName, logError,
                                              logInfo, logWarning)

import           Control.Exception           (catch, throwIO)
import           Control.Monad               (forM, forM_, unless, when)
import           Control.Monad.Reader        (ask)
import           Data.List                   (intercalate, nub)
import           Data.Maybe                  (mapMaybe)
import qualified Data.Text                   as T
import           Data.Time                   (getCurrentTime)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Pipes                       (MonadIO (liftIO), cat, (>->))
import qualified Pipes.Prelude               as P
import           Pipes.Safe                  (SafeT, runSafeT)
import           SequenceFormats.Eigenstrat  (EigenstratSnpEntry (..),
                                              GenoEntry (..), GenoLine,
                                              writeEigenstrat)
import           SequenceFormats.Plink       (writePlink)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (dropTrailingPathSeparator, (<.>),
                                              (</>))

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _forgeGenoSources :: [GenoDataSource]
    -- Empty list = forge all packages
    , _forgeEntityInput :: [EntityInput SignedEntity] -- Empty list = forge all packages
    , _forgeSnpFile     :: Maybe FilePath
    , _forgeIntersect   :: Bool
    , _forgeOutFormat   :: GenotypeFormatSpec
    , _forgeOutMinimal  :: Bool
    , _forgeOutOnlyGeno :: Bool
    , _forgeOutPacPath  :: FilePath
    , _forgeOutPacName  :: Maybe String
    , _forgeNoExtract   :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

-- | The main function running the forge command
runForge :: ForgeOptions -> PoseidonLogIO ()
runForge (
    ForgeOptions genoSources
                 entityInputs maybeSnpFile intersect_
                 outFormat minimal onlyGeno outPathRaw maybeOutName
                 noExtract
    ) = do

    -- load packages --
    properPackages <- readPoseidonPackageCollection pacReadOpts $ [getPacBaseDirs x | x@PacBaseDir {} <- genoSources]
    pseudoPackages <- liftIO $ mapM makePseudoPackageFromGenotypeData $ [getGenoDirect x | x@GenoDirect {} <- genoSources]
    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    let allPackages = properPackages ++ pseudoPackages

    -- compile entities
    entitiesUser <- readEntityInputs entityInputs

    entities <- case entitiesUser of
        [] -> do
            logInfo "No requested entities. Implicitly forging all packages."
            return $ map (Include . Pac . posPacTitle) allPackages
        (Include _:_) -> do
            return entitiesUser
        (Exclude _:_) -> do
            -- fill entitiesToInclude with all packages, if entitiesInput starts with an Exclude
            logInfo "forge entities begin with exclude, so implicitly adding all packages as includes before \
                \applying excludes."
            return $ map (Include . Pac . posPacTitle) allPackages ++ entitiesUser -- add all Packages to the front of the list
    logInfo $ "Forging with the following entity-list: " ++ (intercalate ", " . map show . take 10) entities ++
        if length entities > 10 then " and " ++ show (length entities - 10) ++ " more" else ""

    -- check for entities that do not exist in this dataset
    let nonExistentEntities = findNonExistentEntities entities . getJointIndividualInfo $ allPackages
    unless (null nonExistentEntities) $
        logWarning $ "Detected entities that do not exist in the dataset. They will be ignored: " ++
            intercalate ", " (map show nonExistentEntities)

    -- determine relevant packages
    let relevantPackages = filterRelevantPackages entities allPackages
    logInfo $ (show . length $ relevantPackages) ++ " packages contain data for this forging operation"
    when (null relevantPackages) $ liftIO $ throwIO PoseidonEmptyForgeException

    -- get all individuals from the relevant packages
    let allInds = getJointIndividualInfo $ relevantPackages

    -- determine indizes of relevant individuals and resolve duplicates
    let (unresolvedDuplicatedInds, relevantIndices) = resolveEntityIndices entities allInds

    -- check if there still are duplicates and if yes, then stop
    unless (null unresolvedDuplicatedInds) $ do
        logError "There are duplicated individuals, but forge does not allow that"
        logError "Please specify in your --forgeString or --forgeFile:"
        mapM_ (\(_,i@(IndividualInfo n _ _),_) -> logError $ show (SimpleInd n) ++ " -> " ++ show (SpecificInd i)) $ concat unresolvedDuplicatedInds
        liftIO $ throwIO $ PoseidonForgeEntitiesException "Unresolved duplicated individuals"

    -- collect data --
    -- janno
    let jannoRows = getJointJanno relevantPackages
        relevantJannoRows = map (jannoRows !!) relevantIndices

    -- bib
    let bibEntries = concatMap posPacBib relevantPackages
        relevantBibEntries = filterBibEntries relevantJannoRows bibEntries

    -- create new package --
    let outPath = dropTrailingPathSeparator outPathRaw
    outName <- liftIO $ determinePackageOutName maybeOutName outPath
    -- create new directory
    logInfo $ "Writing to directory (will be created if missing): " ++ outPath
    liftIO $ createDirectoryIfMissing True outPath
    -- compile genotype data structure
    let [outInd, outSnp, outGeno] = case outFormat of
            GenotypeFormatEigenstrat -> [outName <.> ".ind", outName <.> ".snp", outName <.> ".geno"]
            GenotypeFormatPlink -> [outName <.> ".fam", outName <.> ".bim", outName <.> ".bed"]
    -- output warning if any snpSet is set to Other
    snpSetList <- fillMissingSnpSets relevantPackages
    let newSNPSet = case
            maybeSnpFile of
                Nothing -> snpSetMergeList snpSetList intersect_
                Just _  -> SNPSetOther
    let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing (Just newSNPSet)
    -- create package
    logInfo "Creating new package entity"
    pac <- if minimal
           then return $ newMinimalPackageTemplate outPath outName genotypeData
           else liftIO $ newPackageTemplate outPath outName genotypeData (Just (Right relevantJannoRows)) relevantBibEntries

    -- write new package to the file system --
    -- POSEIDON.yml
    unless onlyGeno $ do
        logInfo "Creating POSEIDON.yml"
        liftIO $ writePoseidonPackage pac
    -- bib
    unless (minimal || onlyGeno || null relevantBibEntries) $ do
        logInfo "Creating .bib file"
        liftIO $ writeBibTeXFile (outPath </> outName <.> "bib") relevantBibEntries
    -- genotype data
    logInfo "Compiling genotype data"
    logInfo "Processing SNPs..."
    logEnv <- ask
    currentTime <- liftIO getCurrentTime
    newNrSNPs <- liftIO $ catch (
        runSafeT $ do
            (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData logEnv intersect_ relevantPackages maybeSnpFile
            let eigenstratIndEntriesV = eigenstratIndEntries
            let newEigenstratIndEntries = map (eigenstratIndEntriesV !!) relevantIndices

            let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]
            let outConsumer = case outFormat of
                    GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI newEigenstratIndEntries
                    GenotypeFormatPlink -> writePlink outG outS outI newEigenstratIndEntries
            let extractPipe = if noExtract then cat else P.map (selectIndices relevantIndices)
            -- define main forge pipe including file output.
            -- The final tee forwards the results to be used in the snpCounting-fold
            let forgePipe = eigenstratProd >->
                    printSNPCopyProgress logEnv currentTime >->
                    extractPipe >->
                    P.tee outConsumer
            let startAcc = liftIO $ VUM.replicate (length newEigenstratIndEntries) 0
            P.foldM sumNonMissingSNPs startAcc return forgePipe
        ) (throwIO . PoseidonGenotypeExceptionForward)
    logInfo "Done"
    -- janno (with updated SNP numbers)
    unless (minimal || onlyGeno) $ do
        logInfo "Creating .janno file"
        snpList <- liftIO $ VU.freeze newNrSNPs
        let jannoRowsWithNewSNPNumbers = zipWith (\x y -> x {jNrSNPs = Just y})
                                                relevantJannoRows
                                                (VU.toList snpList)
        liftIO $ writeJannoFile (outPath </> outName <.> "janno") jannoRowsWithNewSNPNumbers

sumNonMissingSNPs :: VUM.IOVector Int -> (EigenstratSnpEntry, GenoLine) -> SafeT IO (VUM.IOVector Int)
sumNonMissingSNPs accumulator (_, geno) = do
    forM_ (zip (V.toList geno) [0..]) $ \(g, i) -> do
        let x = nonMissingToInt g
        VUM.modify accumulator (+x) i
    return accumulator
  where
    nonMissingToInt :: GenoEntry -> Int
    nonMissingToInt x
        | x == Missing = 0
        | otherwise = 1

filterBibEntries :: [JannoRow] -> BibTeX -> BibTeX
filterBibEntries samples references_ =
    let relevantPublications = map T.unpack . nub . concatMap getJannoList . mapMaybe jPublication $ samples
    in filter (\x-> bibEntryId x `elem` relevantPublications) references_

fillMissingSnpSets :: [PoseidonPackage] -> PoseidonLogIO [SNPSetSpec]
fillMissingSnpSets packages = forM packages $ \pac -> do
    let title_ = posPacTitle pac
        maybeSnpSet = snpSet . posPacGenotypeData $ pac
    case maybeSnpSet of
        Just s -> return s
        Nothing -> do
            logWarning $ "Warning for package " ++ title_ ++ ": field \"snpSet\" \
                \is not set. I will interpret this as \"snpSet: Other\""
            return SNPSetOther
