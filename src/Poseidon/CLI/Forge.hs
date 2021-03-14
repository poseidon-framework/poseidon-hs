module Poseidon.CLI.Forge where

import           Poseidon.BibFile           (writeBibTeXFile)
import           Poseidon.EntitiesList      (EntitiesList (..),
                                             PoseidonEntity (..),
                                             readEntitiesFromFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..),
                                             GenotypeFormatSpec (..),
                                             printSNPCopyProgress)
import           Poseidon.Janno             (JannoRow (..),
                                             writeJannoFile)
import           Poseidon.Package           (ContributorSpec (..),
                                             PoseidonPackage (..), getChecksum,
                                             getIndividuals,
                                             getJointGenotypeData,
                                             newPackageTemplate,
                                             readPoseidonPackageCollection,
                                             writePoseidonPackage)
import           Poseidon.Utils             (PoseidonException (..))

import           Control.Monad              (forM, unless, when)
import           Data.List                  (intercalate, intersect, nub,
                                             sortOn, (\\))
import           Data.Maybe                 (catMaybes, isJust, mapMaybe)
import           Data.Text                  (unpack)
import qualified Data.Vector                as V
import           Data.Yaml.Pretty.Extras    (encodeFilePretty)
import           Pipes                      (MonadIO (liftIO), Pipe (..), await,
                                             lift, runEffect, yield, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (SafeT (..), runSafeT, throwM)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..), GenoLine,
                                             writeEigenstrat)
import           SequenceFormats.Plink      (writePlink)
import           System.Console.ANSI        (hClearLine, hSetCursorColumn)
import           System.Directory           (createDirectory)
import           System.FilePath            ((<.>), (</>))
import           System.IO                  (hFlush, hPutStr, hPutStrLn, stderr)
import           Text.CSL.Reference         (Reference (..), refId, unLiteral)

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _jaBaseDirs   :: [FilePath]
    , _entityList   :: EntitiesList
    , _entityFile   :: Maybe FilePath
    , _intersect    :: Bool
    , _outPacPath   :: FilePath
    , _outPacName   :: String
    , _outFormat    :: GenotypeFormatSpec
    , _showWarnings :: Bool
    }

-- | The main function running the forge command
runForge :: ForgeOptions -> IO ()
runForge (ForgeOptions baseDirs entitiesDirect entitiesFile intersect outPath outName outFormat showWarnings) = do
    -- compile entities
    entitiesFromFile <- case entitiesFile of
        Nothing -> return []
        Just f  -> readEntitiesFromFile f
    let entities = entitiesDirect ++ entitiesFromFile
    -- load packages --
    allPackages <- readPoseidonPackageCollection True False baseDirs
    -- check for entities that do not exist this this dataset
    nonExistentEntities <- findNonExistentEntities entities allPackages
    unless (null nonExistentEntities) $
        hPutStrLn stderr $ "The following entities do not exist in this dataset and will be ignored: " ++
            intercalate ", " (map show nonExistentEntities)
    -- determine relevant packages
    relevantPackages <- filterPackages entities allPackages
    hPutStrLn stderr $ (show . length $ relevantPackages) ++ " packages contain data for this forging operation"
    when (null relevantPackages) $
        throwM PoseidonEmptyForgeException
    -- collect data --
    -- janno
    let namesOfRelevantPackages = map posPacTitle relevantPackages
    let jannos = map posPacJanno relevantPackages
    let relevantJannoRows = filterJannoFiles entities $ zip namesOfRelevantPackages jannos
    -- -- bib
    let bibEntries = concatMap posPacBib relevantPackages
    let relevantBibEntries = filterBibEntries relevantJannoRows bibEntries
    -- genotype data
    indices <- extractEntityIndices entities relevantPackages
    -- create new package --
    -- create new directory
    hPutStrLn stderr $ "Creating new package directory: " ++ outPath
    createDirectory outPath
    -- compile genotype data structure
    let [outInd, outSnp, outGeno] = case outFormat of 
            GenotypeFormatEigenstrat -> [outName <.> ".ind", outName <.> ".snp", outName <.> ".geno"]
            GenotypeFormatPlink -> [outName <.> ".fam", outName <.> ".bim", outName <.> ".bed"]
    let genotypeData = GenotypeDataSpec outFormat outGeno Nothing outSnp Nothing outInd Nothing
    -- create new package
    hPutStrLn stderr "Creating new package entity"
    pac <- newPackageTemplate outPath outName genotypeData Nothing (Just relevantJannoRows) (Just relevantBibEntries)
    -- POSEIDON.yml
    hPutStrLn stderr "Creating POSEIDON.yml"
    writePoseidonPackage pac
    -- janno
    hPutStrLn stderr "Creating .janno file"
    writeJannoFile (outPath </> outName <.> "janno") relevantJannoRows
    -- bib
    hPutStrLn stderr "Creating .bib file"
    writeBibTeXFile (outPath </> outName <.> "bib") relevantBibEntries
    -- genotype data
    hPutStrLn stderr "Compiling genotype data"
    runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData showWarnings intersect relevantPackages
        let eigenstratIndEntriesV = V.fromList eigenstratIndEntries
        let newEigenstratIndEntries = [eigenstratIndEntriesV V.! i | i <- indices]
        let jannoIndIds = map jIndividualID relevantJannoRows
        -- TODO: This check might be redundant now, because the input data is now already
        -- screened for cross-file order issues
        when ([n | EigenstratIndEntry n _ _ <-  newEigenstratIndEntries] /= jannoIndIds) $
            throwM (PoseidonCrossFileConsistencyException "new package" "Cannot forge: order of individuals in genotype indidividual files and Janno-files not consistent")
        let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]
        let outConsumer = case outFormat of
                GenotypeFormatEigenstrat -> writeEigenstrat outG outS outI newEigenstratIndEntries
                GenotypeFormatPlink -> writePlink outG outS outI newEigenstratIndEntries
        runEffect $ eigenstratProd >-> printSNPCopyProgress >-> P.map (selectIndices indices) >-> outConsumer
        liftIO $ hClearLine stderr
        liftIO $ hSetCursorColumn stderr 0
        liftIO $ hPutStrLn stderr "SNPs processed: All done"

selectIndices :: [Int] -> (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine)
selectIndices indices (snpEntry, genoLine) = (snpEntry, V.fromList [genoLine V.! i | i <- indices])

findNonExistentEntities :: EntitiesList -> [PoseidonPackage] -> IO [PoseidonEntity]
findNonExistentEntities entities packages = do
    inds <- concat <$> mapM getIndividuals packages
    let titlesPac     = map posPacTitle packages
        indNamesPac   = [ind   | EigenstratIndEntry ind _ _     <- inds]
        groupNamesPac = [group | EigenstratIndEntry _   _ group <- inds]
    let titlesRequestedPacs = [ pac   | Pac   pac   <- entities]
        groupNamesStats     = [ group | Group group <- entities]
        indNamesStats       = [ ind   | Ind   ind   <- entities]
    let missingPacs   = map Pac   $ titlesRequestedPacs \\ titlesPac
        missingInds   = map Ind   $ indNamesStats       \\ indNamesPac
        missingGroups = map Group $ groupNamesStats     \\ groupNamesPac
    return $ missingPacs ++ missingInds ++ missingGroups

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
            ||  length (intersect indNamesPac indNamesStats) > 0
            ||  length (intersect groupNamesPac groupNamesStats) > 0
        then return (Just pac)
        else return Nothing

filterJannoRows :: EntitiesList -> [JannoRow] -> [JannoRow]
filterJannoRows entities samples =
    let groupNamesStats = [ group | Group group <- entities]
        indNamesStats   = [ ind   | Ind   ind   <- entities]
        comparison x    =  jIndividualID x `elem` indNamesStats
                           || head (jGroupName x) `elem` groupNamesStats
    in filter comparison samples

filterJannoFiles :: EntitiesList -> [(String, [JannoRow])] -> [JannoRow]
filterJannoFiles entities packages =
    let requestedPacs           = [ pac | Pac pac <- entities]
        filterJannoOrNot (a, b) = if a `elem` requestedPacs
                                  then b
                                  else filterJannoRows entities b
    in concatMap filterJannoOrNot packages

filterBibEntries :: [JannoRow] -> [Reference] -> [Reference]
filterBibEntries samples references =
    let relevantPublications = nub $ mapMaybe jPublication samples
    in filter (\x-> (unpack . unLiteral . refId) x `elem` relevantPublications) references

extractEntityIndices :: EntitiesList -> [PoseidonPackage] -> IO [Int]
extractEntityIndices entities relevantPackages = do
    let pacNames   = [ pac   | Pac   pac   <- entities]
        groupNames = [ group | Group group <- entities]
        indNames   = [ ind   | Ind   ind   <- entities]
    let allPackageNames = map posPacTitle relevantPackages
    allIndEntries <- mapM getIndividuals relevantPackages
    let filterFunc (_ , pacName, EigenstratIndEntry ind _ group) =
            pacName `elem` pacNames || ind `elem` indNames || group `elem` groupNames
    return $ map extractFirst $ filter filterFunc (zipGroup allPackageNames allIndEntries)

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

zipGroup :: [a] -> [[b]] -> [(Int,a,b)]
zipGroup list nestedList =
    let lenghtsNestedList = map length nestedList
        listWithlenghtsNestedList = zip lenghtsNestedList list
        longerA = map (uncurry replicate) listWithlenghtsNestedList
    in zip3 [0..] (concat longerA) (concat nestedList)
