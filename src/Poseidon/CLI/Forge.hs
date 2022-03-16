module Poseidon.CLI.Forge where

import           Poseidon.BibFile            (BibEntry (..), BibTeX,
                                              writeBibTeXFile)
import           Poseidon.EntitiesList       (PoseidonEntity (..), SignedEntity(..),
                                              SignedEntitiesList,
                                              readEntitiesFromFile,
                                              findNonExistentEntities,
                                              filterRelevantPackages,
                                              conformingEntityIndices)
import           Poseidon.GenotypeData       (GenotypeDataSpec (..),
                                              GenotypeFormatSpec (..),
                                              SNPSetSpec (..),
                                              printSNPCopyProgress,
                                              selectIndices, snpSetMergeList,
                                              selectIndices)
import           Poseidon.Janno              (JannoList (..), JannoRow (..),
                                              writeJannoFile)
import           Poseidon.Package            (PackageReadOptions (..),
                                              PoseidonPackage (..),
                                              defaultPackageReadOptions,
                                              getJointGenotypeData,
                                              newMinimalPackageTemplate,
                                              newPackageTemplate,
                                              readPoseidonPackageCollection,
                                              writePoseidonPackage,
                                              getJointIndividualInfo,
                                              getJointJanno)
import           Poseidon.Utils              (PoseidonException (..))

import           Control.Monad               (forM, forM_, unless, when)
import           Data.List                   (intercalate, nub, (\\))
import           Data.Maybe                  (mapMaybe)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Pipes                       (MonadIO (liftIO), cat, (>->))
import qualified Pipes.Prelude               as P
import           Pipes.Safe                  (SafeT, runSafeT, throwM)
import           SequenceFormats.Eigenstrat  (EigenstratSnpEntry (..),
                                              GenoEntry (..), GenoLine,
                                              writeEigenstrat)
import           SequenceFormats.Plink       (writePlink)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (takeBaseName, (<.>), (</>))
import           System.IO                   (hPutStrLn, stderr)

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _forgeBaseDirs     :: [FilePath]
    , _forgeEntitySpec   :: Either SignedEntitiesList FilePath
    , _forgeIntersect    :: Bool
    , _forgeOutPacPath   :: FilePath
    , _forgeOutPacName   :: Maybe String
    , _forgeOutFormat    :: GenotypeFormatSpec
    , _forgeOutMinimal   :: Bool
    , _forgeShowWarnings :: Bool
    , _forgeNoExtract    :: Bool
    , _forgeSnpFile      :: Maybe FilePath
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptVerbose          = False
    , _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = False
    , _readOptGenoCheck        = True
    }

-- | The main function running the forge command
runForge :: ForgeOptions -> IO ()
runForge (ForgeOptions baseDirs entitySpec intersect_ outPath maybeOutName outFormat minimal showWarnings noExtract maybeSnpFile) = do
    
    -- this message can be removed after a couple of releases
    hPutStrLn stderr 
      "The semantics of --forgeString and --forgeFile have been changed in trident v0.27.0. \
      \Removing samples, groups or packages now follows a different logic. Please see the \
      \documentation in trident forge -h to verify that your selection still behaves as you expect."
    hPutStrLn stderr ""

    -- compile entities
    entitiesInput <- case entitySpec of
        Left e -> return e
        Right fp -> readEntitiesFromFile fp

    hPutStrLn stderr $ "Forging with the following entity-list: " ++ show entitiesInput
    
    -- load packages --
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    
    -- fill entitiesToInclude with all packages, if entitiesInput starts with an Exclude
    let addImplicits = do
            hPutStrLn stderr $ "forge entities begin with exclude or are empty, so implicitly adding all packages as includes before \
            \applying excludes."
            return $ map (Include . Pac . posPacTitle) allPackages ++ entitiesInput -- add all Packages to the front of the list
    entities <- case entitiesInput of
        (Include _:_) -> return entitiesInput
        (Exclude _:_) -> addImplicits
        []            -> addImplicits
    
    -- check for entities that do not exist this this dataset
    let nonExistentEntities = findNonExistentEntities entities . getJointIndividualInfo $ allPackages
    unless (null nonExistentEntities) $
        hPutStrLn stderr $ "The following entities do not exist in this dataset and will be ignored: " ++
            intercalate ", " (map show nonExistentEntities)
    
    -- determine relevant packages
    let relevantPackages = filterRelevantPackages entities allPackages
    hPutStrLn stderr $ (show . length $ relevantPackages) ++ " packages contain data for this forging operation"
    when (null relevantPackages) $ throwM PoseidonEmptyForgeException

    -- determine relevant individual indices
    let relevantIndices = conformingEntityIndices entities . getJointIndividualInfo $ relevantPackages

    -- collect data --
    -- janno
    let jannoRows = getJointJanno relevantPackages
        relevantJannoRows = map (jannoRows !!) relevantIndices
    
    -- check for duplicates among the individuals selected for merging
    checkIndividualsUniqueJanno relevantJannoRows
    -- bib
    let bibEntries = concatMap posPacBib relevantPackages
        relevantBibEntries = filterBibEntries relevantJannoRows bibEntries
    -- create new package --
    let outName = case maybeOutName of -- take basename of outPath, if name is not provided
            Just x  -> x
            Nothing -> takeBaseName outPath
    when (outName == "") $ throwM PoseidonEmptyOutPacNameException
    -- create new directory
    hPutStrLn stderr $ "Creating new package directory: " ++ outPath
    createDirectoryIfMissing True outPath
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
    -- create new package
    hPutStrLn stderr "Creating new package entity"
    pac <- if minimal
           then return $ newMinimalPackageTemplate outPath outName genotypeData
           else newPackageTemplate outPath outName genotypeData (Just (Right relevantJannoRows)) relevantBibEntries
    -- POSEIDON.yml
    hPutStrLn stderr "Creating POSEIDON.yml"
    writePoseidonPackage pac
    -- bib
    unless (minimal || null relevantBibEntries) $ do
        hPutStrLn stderr "Creating .bib file"
        writeBibTeXFile (outPath </> outName <.> "bib") relevantBibEntries
    -- genotype data
    hPutStrLn stderr "Compiling genotype data"
    newNrSNPs <- runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData showWarnings intersect_ relevantPackages maybeSnpFile
        let eigenstratIndEntriesV = eigenstratIndEntries
        let newEigenstratIndEntries = map (eigenstratIndEntriesV !!) relevantIndices

        let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]
        let outConsumer = case outFormat of
                GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI newEigenstratIndEntries
                GenotypeFormatPlink -> writePlink outG outS outI newEigenstratIndEntries
        liftIO $ hPutStrLn stderr "Processing SNPs..."
        let extractPipe = if noExtract then cat else P.map (selectIndices relevantIndices)
        -- define main forge pipe including file output.
        -- The final tee forwards the results to be used in the snpCounting-fold
        let forgePipe = eigenstratProd >->
                printSNPCopyProgress >->
                extractPipe >->
                P.tee outConsumer

        let startAcc = liftIO $ VUM.replicate (length newEigenstratIndEntries) 0
        P.foldM sumNonMissingSNPs startAcc return forgePipe
    -- janno (with updated SNP numbers)
    liftIO $ hPutStrLn stderr "Done"
    unless minimal $ do
        hPutStrLn stderr "Creating .janno file"
        snpList <- VU.freeze newNrSNPs
        let jannoRowsWithNewSNPNumbers = zipWith (\x y -> x {jNrSNPs = Just y})
                                                relevantJannoRows
                                                (VU.toList snpList)
        writeJannoFile (outPath </> outName <.> "janno") jannoRowsWithNewSNPNumbers


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

checkIndividualsUniqueJanno :: [JannoRow] -> IO ()
checkIndividualsUniqueJanno rows = do
    let indIDs = map jPoseidonID rows
    when (length indIDs /= length (nub indIDs)) $ do
        throwM $ PoseidonForgeEntitiesException $
            "Duplicate individuals in selection (" ++
            intercalate ", " (indIDs \\ nub indIDs) ++
            ")"

filterBibEntries :: [JannoRow] -> BibTeX -> BibTeX
filterBibEntries samples references_ =
    let relevantPublications = nub . concatMap getJannoList . mapMaybe jPublication $ samples
    in filter (\x-> bibEntryId x `elem` relevantPublications) references_

fillMissingSnpSets :: [PoseidonPackage] -> IO [SNPSetSpec]
fillMissingSnpSets packages = forM packages $ \pac -> do
    let title_ = posPacTitle pac
        maybeSnpSet = snpSet . posPacGenotypeData $ pac
    case maybeSnpSet of
        Just s -> return s
        Nothing -> do
            hPutStrLn stderr ("Warning for package " ++ title_ ++ ": field \"snpSet\" \
            \is not set. I will interpret this as \"snpSet: Other\"")
            return SNPSetOther
