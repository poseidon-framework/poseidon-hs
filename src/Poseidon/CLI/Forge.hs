{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Forge where

import           Poseidon.BibFile            (BibEntry (..), BibTeX,
                                              writeBibTeXFile)
import           Poseidon.EntityTypes        (EntityInput,
                                              PacNameAndVersion (..),
                                              PoseidonEntity (..),
                                              SignedEntity (..),
                                              checkIfAllEntitiesExist,
                                              isLatestInCollection,
                                              makePacNameAndVersion,
                                              readEntityInputs,
                                              resolveUniqueEntityIndices)
import           Poseidon.GenotypeData       (GenoDataSource (..),
                                              GenotypeDataSpec (..),
                                              GenotypeFormatSpec (..),
                                              SNPSetSpec (..),
                                              printSNPCopyProgress,
                                              selectIndices, snpSetMergeList)
import           Poseidon.Janno              (JannoList (..), JannoRow (..),
                                              JannoRows (..), getMaybeJannoList,
                                              writeJannoFile)
import           Poseidon.Package            (PackageReadOptions (..),
                                              PoseidonPackage (..),
                                              defaultPackageReadOptions,
                                              filterToRelevantPackages,
                                              getJointGenotypeData,
                                              getJointIndividualInfo,
                                              getJointJanno,
                                              makePseudoPackageFromGenotypeData,
                                              newMinimalPackageTemplate,
                                              newPackageTemplate,
                                              readPoseidonPackageCollection,
                                              writePoseidonPackage)
import           Poseidon.SequencingSource   (SeqSourceRow (..),
                                              SeqSourceRows (..),
                                              writeSeqSourceFile)
import           Poseidon.Utils              (PoseidonException (..),
                                              PoseidonIO, checkFile,
                                              determinePackageOutName,
                                              envInputPlinkMode, envLogAction,
                                              logInfo, logWarning, uniqueRO)

import           Control.Exception           (catch, throwIO)
import           Control.Monad               (filterM, forM, forM_, unless,
                                              when)
import           Data.List                   (intercalate, nub)
import           Data.Maybe                  (mapMaybe)
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
import           SequenceFormats.Plink       (PlinkPopNameMode,
                                              eigenstratInd2PlinkFam,
                                              writePlink)
import           System.Directory            (copyFile,
                                              createDirectoryIfMissing)
import           System.FilePath             (dropTrailingPathSeparator, (<.>),
                                              (</>))

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _forgeGenoSources        :: [GenoDataSource]
    -- Empty list = forge all packages
    , _forgeEntityInput        :: [EntityInput SignedEntity] -- Empty list = forge all packages
    , _forgeSnpFile            :: Maybe FilePath
    , _forgeIntersect          :: Bool
    , _forgeOutFormat          :: GenotypeFormatSpec
    , _forgeOutMode            :: ForgeOutMode
    , _forgeOutPacPath         :: FilePath
    , _forgeOutPacName         :: Maybe String
    , _forgePackageWise        :: Bool
    , _forgeOutputPlinkPopMode :: PlinkPopNameMode
    , _forgeOutputOrdered      :: Bool
    }

-- | Different output modes ordered from more minimal to more complete
data ForgeOutMode =
      GenoOut
    | MinimalOut
    | PreservePymlOut
    | NormalOut

onlyGeno :: ForgeOutMode -> Bool
onlyGeno GenoOut = True
onlyGeno _       = False
minimal :: ForgeOutMode -> Bool
minimal MinimalOut = True
minimal _          = False
preservePyml :: ForgeOutMode -> Bool
preservePyml PreservePymlOut = True
preservePyml _               = False

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

-- | The main function running the forge command
runForge :: ForgeOptions -> PoseidonIO ()
runForge (
    ForgeOptions genoSources
                 entityInputs maybeSnpFile intersect_
                 outFormat outMode outPathRaw maybeOutName
                 packageWise outPlinkPopMode
                 outputOrdered
    ) = do

    -- load packages --
    properPackages <- readPoseidonPackageCollection pacReadOpts $ [getPacBaseDirs x | x@PacBaseDir {} <- genoSources]
    pseudoPackages <- mapM makePseudoPackageFromGenotypeData [getGenoDirect x | x@GenoDirect {} <- genoSources]
    logInfo $ "Unpackaged genotype data files loaded: " ++ show (length pseudoPackages)
    let allPackages = properPackages ++ pseudoPackages

    -- compile entities
    entitiesUser <- readEntityInputs entityInputs

    allLatestPackages <- filterM (isLatestInCollection allPackages) allPackages
    entities <- case entitiesUser of
        [] -> do
            logInfo "No requested entities. Implicitly forging all packages."
            return $ map (Include . Pac . makePacNameAndVersion) allLatestPackages
        (Include _:_) -> do
            return entitiesUser
        (Exclude _:_) -> do
            -- fill entitiesToInclude with all packages, if entitiesInput starts with an Exclude
            logInfo "forge entities begin with exclude, so implicitly adding all packages \
                \(latest versions) as includes before applying excludes."
            -- add all latest packages to the front of the list
            return $ map (Include . Pac . makePacNameAndVersion) allLatestPackages ++ entitiesUser
    logInfo $ "Forging with the following entity-list: " ++ (intercalate ", " . map show . take 10) entities ++
        if length entities > 10 then " and " ++ show (length entities - 10) ++ " more" else ""

    -- check if all entities can be found. This function reports an error and throws and exception
    getJointIndividualInfo allPackages >>= checkIfAllEntitiesExist entities
    -- determine relevant packages
    relevantPackages <- filterToRelevantPackages entities allPackages
    logInfo $ (show . length $ relevantPackages) ++ " packages contain data for this forging operation"
    when (null relevantPackages) $ liftIO $ throwIO PoseidonEmptyForgeException
    when (length relevantPackages > 1 && preservePyml outMode) $ liftIO $ throwIO PoseidonCantPreserveException

    -- get all individuals from the relevant packages
    indInfoCollection <- getJointIndividualInfo relevantPackages

    -- set entities to only packages, if --packagewise is set
    let relevantEntities =
            if packageWise
            then map (Include . Pac . makePacNameAndVersion) relevantPackages
            else entities

    -- determine indizes of relevant individuals
    relevantIndices <- resolveUniqueEntityIndices outputOrdered relevantEntities indInfoCollection

    -- collect data --
    -- janno
    let (JannoRows jannoRows) = getJointJanno relevantPackages
        newJanno@(JannoRows relevantJannoRows) = JannoRows $ map (jannoRows !!) relevantIndices

    -- seqSource
    let seqSourceRows = mconcat $ map posPacSeqSource relevantPackages
        relevantSeqSourceRows = filterSeqSourceRows newJanno seqSourceRows

    -- bib
    let bibEntries = uniqueRO $ concatMap posPacBib relevantPackages
        relevantBibEntries = filterBibEntries newJanno bibEntries

    -- create new package --
    let outPath = dropTrailingPathSeparator outPathRaw
    outName <- liftIO $ determinePackageOutName maybeOutName outPath
    -- create new directory
    logInfo $ "Writing to directory (will be created if missing): " ++ outPath
    liftIO $ createDirectoryIfMissing True outPath
    -- compile genotype data structure
    let (outInd, outSnp, outGeno) = case outFormat of
            GenotypeFormatEigenstrat -> (outName <.> ".ind", outName <.> ".snp", outName <.> ".geno")
            GenotypeFormatPlink -> (outName <.> ".fam", outName <.> ".bim", outName <.> ".bed")
    -- output warning if any snpSet is set to Other
    snpSetList <- fillMissingSnpSets relevantPackages
    let newSNPSet = case
            maybeSnpFile of
                Nothing -> snpSetMergeList snpSetList intersect_
                Just _  -> SNPSetOther
    let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing (Just newSNPSet)

    -- assemble and write result depending on outMode --
    logInfo "Creating new package entity"
    let pacSource = head relevantPackages
    case outMode of
        GenoOut -> do
            _ <- compileGenotypeData outPath (outInd, outSnp, outGeno) relevantPackages relevantIndices
            return ()
        MinimalOut -> do
            let pac = newMinimalPackageTemplate outPath outName genotypeData
            writePoseidonYmlFile pac
            _ <- compileGenotypeData outPath (outInd, outSnp, outGeno) relevantPackages relevantIndices
            return ()
        PreservePymlOut -> do
            normalPac <- newPackageTemplate outPath outName genotypeData
                    (Just (Right newJanno)) relevantSeqSourceRows relevantBibEntries
            let pac = normalPac {
                    posPacNameAndVersion = (posPacNameAndVersion pacSource) {panavName = outName}
                ,   posPacDescription    = posPacDescription pacSource
                ,   posPacLastModified   = posPacLastModified pacSource
                ,   posPacContributor    = posPacContributor pacSource
                ,   posPacReadmeFile     = posPacReadmeFile pacSource
                ,   posPacChangelogFile  = posPacChangelogFile pacSource
                }
            writePoseidonYmlFile pac
            writeSSFile outPath outName relevantSeqSourceRows
            writeBibFile outPath outName relevantBibEntries
            copyREADMEFile outPath pacSource
            copyCHANGELOGFile outPath pacSource
            newNrSnps <- compileGenotypeData outPath (outInd, outSnp, outGeno) relevantPackages relevantIndices
            writingJannoFile outPath outName newNrSnps relevantJannoRows
        NormalOut  -> do
            pac <- newPackageTemplate outPath outName genotypeData
                    (Just (Right newJanno)) relevantSeqSourceRows relevantBibEntries
            writePoseidonYmlFile pac
            writeSSFile outPath outName relevantSeqSourceRows
            writeBibFile outPath outName relevantBibEntries
            newNrSnps <- compileGenotypeData outPath (outInd, outSnp, outGeno) relevantPackages relevantIndices
            writingJannoFile outPath outName newNrSnps relevantJannoRows

    where
        -- individual writer functions --
        writePoseidonYmlFile :: PoseidonPackage -> PoseidonIO ()
        writePoseidonYmlFile pac = do
            logInfo "Creating POSEIDON.yml"
            liftIO $ writePoseidonPackage pac
        writeSSFile :: FilePath -> String -> SeqSourceRows -> PoseidonIO ()
        writeSSFile outPath outName rows = do
            unless (null (getSeqSourceRowList rows)) $ do
                logInfo "Creating .ssf file"
                liftIO $ writeSeqSourceFile (outPath </> outName <.> "ssf") rows
        writeBibFile :: FilePath -> String -> BibTeX -> PoseidonIO ()
        writeBibFile outPath outName entries = do
            unless (null entries) $ do
                logInfo "Creating .bib file"
                liftIO $ writeBibTeXFile (outPath </> outName <.> "bib") entries
        copyREADMEFile :: FilePath -> PoseidonPackage -> PoseidonIO ()
        copyREADMEFile outPath pacSource = do
            case posPacReadmeFile pacSource of
                Nothing -> return ()
                (Just path) -> do
                    logInfo "Copying README file from source package"
                    let fullSourcePath = posPacBaseDir pacSource </> path
                    liftIO $ checkFile fullSourcePath Nothing
                    liftIO $ copyFile fullSourcePath $ outPath </> path
        copyCHANGELOGFile :: FilePath -> PoseidonPackage -> PoseidonIO ()
        copyCHANGELOGFile outPath pacSource = do
            case posPacReadmeFile pacSource of
                Nothing -> return ()
                (Just path) -> do
                    logInfo "Copying CHANGELOG file from source package"
                    let fullSourcePath = posPacBaseDir pacSource </> path
                    liftIO $ checkFile fullSourcePath Nothing
                    liftIO $ copyFile fullSourcePath $ outPath </> path
        compileGenotypeData :: FilePath -> (String,String,String) -> [PoseidonPackage] -> [Int] ->  PoseidonIO (VUM.IOVector Int)
        compileGenotypeData outPath (outInd, outSnp, outGeno) relevantPackages relevantIndices = do
            logInfo "Compiling genotype data"
            logInfo "Processing SNPs..."
            logA <- envLogAction
            inPlinkPopMode <- envInputPlinkMode
            currentTime <- liftIO getCurrentTime
            newNrSNPs <- liftIO $ catch (
                runSafeT $ do
                    (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData logA intersect_ inPlinkPopMode relevantPackages maybeSnpFile
                    let newEigenstratIndEntries = map (eigenstratIndEntries !!) relevantIndices
                    let (outG, outS, outI) = (outPath </> outGeno, outPath </> outSnp, outPath </> outInd)
                    let outConsumer = case outFormat of
                            GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI newEigenstratIndEntries
                            GenotypeFormatPlink -> writePlink outG outS outI (map (eigenstratInd2PlinkFam outPlinkPopMode) newEigenstratIndEntries)
                    let extractPipe = if packageWise then cat else P.map (selectIndices relevantIndices)
                    -- define main forge pipe including file output.
                    -- The final tee forwards the results to be used in the snpCounting-fold
                    let forgePipe = eigenstratProd >->
                            printSNPCopyProgress logA currentTime >->
                            extractPipe >->
                            P.tee outConsumer
                    let startAcc = liftIO $ VUM.replicate (length newEigenstratIndEntries) 0
                    P.foldM sumNonMissingSNPs startAcc return forgePipe
                ) (throwIO . PoseidonGenotypeExceptionForward)
            logInfo "Done"
            return newNrSNPs
        writingJannoFile :: FilePath -> String -> (VUM.MVector VUM.RealWorld Int) -> [JannoRow] -> PoseidonIO ()
        writingJannoFile outPath outName newNrSNPs rows = do
            logInfo "Creating .janno file"
            snpList <- liftIO $ VU.freeze newNrSNPs
            let jannoRowsWithNewSNPNumbers =
                    zipWith (\x y -> x {jNrSNPs = Just y}) rows (VU.toList snpList)
            liftIO $ writeJannoFile (outPath </> outName <.> "janno") (JannoRows jannoRowsWithNewSNPNumbers)

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

filterSeqSourceRows :: JannoRows -> SeqSourceRows -> SeqSourceRows
filterSeqSourceRows (JannoRows jRows) (SeqSourceRows sRows) =
    let desiredPoseidonIDs = map jPoseidonID jRows
    in SeqSourceRows $ filter (hasAPoseidonID desiredPoseidonIDs) sRows
    where
        hasAPoseidonID :: [String] -> SeqSourceRow -> Bool
        hasAPoseidonID jIDs seqSourceRow =
            let sIDs = getMaybeJannoList $ sPoseidonID seqSourceRow
            in any (`elem` jIDs) sIDs

filterBibEntries :: JannoRows -> BibTeX -> BibTeX
filterBibEntries (JannoRows rows) references_ =
    let relevantPublications = nub . concatMap getJannoList . mapMaybe jPublication $ rows
    in filter (\x-> bibEntryId x `elem` relevantPublications) references_

fillMissingSnpSets :: [PoseidonPackage] -> PoseidonIO [SNPSetSpec]
fillMissingSnpSets packages = forM packages $ \pac -> do
    let pac_ = posPacNameAndVersion pac
        maybeSnpSet = snpSet . posPacGenotypeData $ pac
    case maybeSnpSet of
        Just s -> return s
        Nothing -> do
            logWarning $ "Warning for package " ++ show pac_ ++ ": field \"snpSet\" \
                \is not set. I will interpret this as \"snpSet: Other\""
            return SNPSetOther
