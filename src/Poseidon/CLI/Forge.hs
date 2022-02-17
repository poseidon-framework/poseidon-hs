module Poseidon.CLI.Forge where

import           Poseidon.BibFile            (BibEntry (..), BibTeX,
                                              writeBibTeXFile)
import           Poseidon.EntitiesList       (EntitiesList, PoseidonEntity (..),
                                              SignedEntitiesList,
                                              entityExcludes, entityIncludes,
                                              readSignedEntitiesFromFile,
                                              findNonExistentEntities)
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
                                              writePoseidonPackage)
import           Poseidon.Utils              (PoseidonException (..))

import           Control.Monad               (forM, forM_, unless, when)
import           Data.List                   (intercalate, intersect, nub, (\\))
import           Data.Maybe                  (catMaybes, mapMaybe)
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import           Pipes                       (MonadIO (liftIO), cat, (>->))
import qualified Pipes.Prelude               as P
import           Pipes.Safe                  (SafeT, runSafeT, throwM)
import           SequenceFormats.Eigenstrat  (EigenstratIndEntry (..),
                                              EigenstratSnpEntry (..),
                                              GenoEntry (..), GenoLine,
                                              writeEigenstrat)
import           SequenceFormats.Plink       (writePlink)
import           System.Directory            (createDirectoryIfMissing)
import           System.FilePath             (takeBaseName, (<.>), (</>))
import           System.IO                   (hPutStrLn, stderr)

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _forgeBaseDirs     :: [FilePath]
    , _forgeEntityList   :: SignedEntitiesList
    , _forgeEntityFiles  :: [FilePath]
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
runForge (ForgeOptions baseDirs entitiesDirect entitiesFile intersect_ outPath maybeOutName outFormat minimal showWarnings noExtract maybeSnpFile) = do
    -- compile entities
    entitiesFromFile <- mapM readSignedEntitiesFromFile entitiesFile
    let entities = nub $ entitiesDirect ++ concat entitiesFromFile
        entitiesToIncludePreliminary = entityIncludes entities
        entitiesToExclude = entityExcludes entities
    -- load packages --
    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    -- fill entitiesToInclude with all packages, if entitiesToIncludePreliminary is empty
    let entitiesToInclude = if null entitiesToIncludePreliminary
                            then map (Pac . posPacTitle) allPackages
                            else entitiesToIncludePreliminary
    -- check for entities that do not exist this this dataset
    nonExistentEntities <- findNonExistentEntities (entitiesToInclude ++ entitiesToExclude) allPackages
    unless (null nonExistentEntities) $
        hPutStrLn stderr $ "The following entities do not exist in this dataset and will be ignored: " ++
            intercalate ", " (map show nonExistentEntities)
    -- determine relevant packages
    relevantPackages <- filterPackages (entitiesToInclude \\ entitiesToExclude) allPackages
    hPutStrLn stderr $ (show . length $ relevantPackages) ++ " packages contain data for this forging operation"
    when (null relevantPackages) $ throwM PoseidonEmptyForgeException
    -- collect data --
    -- janno
    let pacNameJannoRows = zip (map posPacTitle relevantPackages) (map posPacJanno relevantPackages)
        jannoRowsToInclude = filterJannoFiles entitiesToInclude pacNameJannoRows
        jannoRowsToExluce = filterJannoFiles entitiesToExclude pacNameJannoRows
        relevantJannoRows = jannoRowsToInclude \\ jannoRowsToExluce
    -- check for duplicates among the individuals selected for merging
    checkIndividualsUniqueJanno relevantJannoRows
    -- bib
    let bibEntries = concatMap posPacBib relevantPackages
        relevantBibEntries = filterBibEntries relevantJannoRows bibEntries
    -- genotype data individual indizes
    indicesToInclude <- extractEntityIndices entitiesToInclude relevantPackages
    indicesToExclude <- extractEntityIndices entitiesToExclude relevantPackages
    let indices = indicesToInclude \\ indicesToExclude
    -- create new package --
    let outName = case maybeOutName of -- take basename of outPath, if name is not provided
            Just x  -> x
            Nothing -> takeBaseName outPath
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
        let eigenstratIndEntriesV = V.fromList eigenstratIndEntries
        let newEigenstratIndEntries = [eigenstratIndEntriesV V.! i | i <- indices]
        let jannoIndIds = map jPoseidonID relevantJannoRows
        -- TODO: This check might be redundant now, because the input data is now already
        -- screened for cross-file order issues
        when ([n | EigenstratIndEntry n _ _ <-  newEigenstratIndEntries] /= jannoIndIds) $
            throwM (PoseidonCrossFileConsistencyException "new package" "Cannot forge: order of individuals in genotype indidividual files and Janno-files not consistent")
        let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]
        let outConsumer = case outFormat of
                GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI newEigenstratIndEntries
                GenotypeFormatPlink -> writePlink outG outS outI newEigenstratIndEntries
        liftIO $ hPutStrLn stderr "Processing SNPs..."
        let extractPipe = if noExtract then cat else P.map (selectIndices indices)
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

filterPackages :: EntitiesList -> [PoseidonPackage] -> IO [PoseidonPackage]
filterPackages entities packages = do
    let requestedPacs   = [ pac   | Pac   pac   <- entities]
        groupNamesStats = [ group | Group group <- entities]
        indNamesStats   = [ ind   | Ind   ind   <- entities]
    fmap catMaybes . forM packages $ \pac -> do
        inds <- getIndividuals pac
        let indNamesPac   = [ind   | EigenstratIndEntry ind _ _     <- inds]
            groupNamesPac = [group | EigenstratIndEntry _   _ group <- inds]
        if  posPacTitle pac `elem` requestedPacs
            ||  not (null (indNamesPac `intersect` indNamesStats))
            ||  not (null (groupNamesPac `intersect` groupNamesStats))
        then return (Just pac)
        else return Nothing

filterJannoRows :: EntitiesList -> [JannoRow] -> [JannoRow]
filterJannoRows entities samples =
    let groupNamesStats = [ group | Group group <- entities]
        indNamesStats   = [ ind   | Ind   ind   <- entities]
        comparison x    =  jPoseidonID x `elem` indNamesStats
                           || head (getJannoList . jGroupName $ x) `elem` groupNamesStats
    in filter comparison samples

filterJannoFiles :: EntitiesList -> [(String, [JannoRow])] -> [JannoRow]
filterJannoFiles entities packages =
    let requestedPacs           = [ pac | Pac pac <- entities]
        filterJannoOrNot (a, b) = if a `elem` requestedPacs
                                  then b
                                  else filterJannoRows entities b
    in concatMap filterJannoOrNot packages

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
