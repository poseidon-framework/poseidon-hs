module Poseidon.CLI.Forge where

import           Poseidon.BibFile           (bibToSimpleMaybeList,
                                             writeBibTeXFile)
import           Poseidon.ForgeRecipe       (ForgeEntity (..), ForgeRecipe (..), 
                                             readEntitiesFromFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), 
                                             GenotypeFormatSpec (..))
import           Poseidon.Janno             (jannoToSimpleMaybeList,
                                             PoseidonSample (..),
                                             writeJannoFile)
import           Poseidon.Package           (ContributorSpec (..),
                                             PoseidonPackage (..),
                                             getIndividuals,
                                             getJointGenotypeData,
                                             readPoseidonPackageCollection,
                                             newPackageTemplate)
import           Poseidon.Utils             (PoseidonException(..))

import           Control.Monad              (when, forM, unless)
import           Data.Yaml.Pretty.Extras    (encodeFilePretty)
import           Data.List                  ((\\), nub, sortOn, intersect, intercalate)
import           Data.Maybe                 (catMaybes, isJust, mapMaybe)
import           Data.Text                  (unpack)
import qualified Data.Vector as V
import           Pipes                      (MonadIO(liftIO), runEffect, (>->), await, yield, lift, Pipe (..))
import qualified Pipes.Prelude as P
import           Pipes.Safe                 (runSafeT, throwM, SafeT (..))
import           SequenceFormats.Eigenstrat (writeEigenstrat, EigenstratIndEntry (..), EigenstratSnpEntry(..), GenoLine)
import           System.Console.ANSI        (clearLine, setCursorColumn)
import           System.Directory           (createDirectory)
import           System.FilePath            ((<.>), (</>))
import           System.IO                  (hPutStrLn, stderr, hFlush, stdout)
import           Text.CSL.Reference         (refId, unLiteral, Reference (..))

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _jaBaseDirs :: [FilePath]
    , _entityList :: ForgeRecipe
    , _entityFile :: Maybe FilePath
    , _outPacPath :: FilePath
    , _outPacName :: String
    , _showWarnings :: Bool
    }

-- | The main function running the forge command
runForge :: ForgeOptions -> IO ()
runForge (ForgeOptions baseDirs entitiesDirect entitiesFile outPath outName showWarnings) = do
    -- compile entities
    entitiesFromFile <- case entitiesFile of
        Nothing -> return []
        Just f -> readEntitiesFromFile f
    let entities = entitiesDirect ++ entitiesFromFile
    -- load packages --
    allPackages <- readAllPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ allPackages) ++ " Poseidon packages found"
    -- check for entities that do not exist this this dataset
    nonExistentEntities <- findNonExistentEntities entities allPackages
    unless (null nonExistentEntities) $
        putStrLn $ "The following entities do not exist in this dataset and will be ignored: " ++
        intercalate ", " (map show nonExistentEntities)
    -- determine relevant packages
    relevantPackages <- filterPackages entities allPackages
    putStrLn $ (show . length $ relevantPackages) ++ " packages contain data for this forging operation"
    -- collect data --
    -- janno
    jannoFiles <- maybeLoadJannoFiles relevantPackages
    let jannoMaybeList = jannoToSimpleMaybeList jannoFiles
    let anyJannoIssues = not $ all isJust jannoMaybeList
    let goodJannoRows = catMaybes jannoMaybeList
    let namesOfRelevantPackages = map posPacTitle relevantPackages
    let relevantJannoRows = filterJannoFiles entities $ zip namesOfRelevantPackages goodJannoRows
    -- bib
    bibFiles <- maybeLoadBibTeXFiles relevantPackages
    let bibMaybeList = bibToSimpleMaybeList bibFiles
    let anyBibIssues = not $ all isJust bibMaybeList
    let goodBibEntries = nub $ sortOn (show . refId) $ concat $ catMaybes bibMaybeList
    let relevantBibEntries = filterBibEntries relevantJannoRows goodBibEntries
    -- genotype data
    indices <- extractEntityIndices entities relevantPackages
    -- print read issue warning
    when (anyJannoIssues || anyBibIssues) $
        putStrLn "\nThere were issues with incomplete, missing or invalid data. Run trident validate to learn more."
    -- create new package --
    putStrLn $ "Creating new package directory: " ++ outPath
    createDirectory outPath
    let outInd = outName <.> "eigenstrat.ind"
        outSnp = outName <.> "eigenstrat.snp"
        outGeno = outName <.> "eigenstrat.geno"
        genotypeData = GenotypeDataSpec GenotypeFormatEigenstrat outGeno outSnp outInd
        outJanno = outName <.> "janno"
        outBib = outName <.> "bib"
    -- POSEIDON.yml
    putStrLn "Compiling POSEIDON.yml"
    pac <- newPackageTemplate outName genotypeData outJanno outBib
    encodeFilePretty (outPath </> "POSEIDON.yml") pac
    -- janno
    putStrLn "Compiling .janno file"
    writeJannoFile (outPath </> outJanno) relevantJannoRows
    -- bib
    putStrLn "Compiling .bib file"
    writeBibTeXFile (outPath </> outBib) relevantBibEntries
    -- genotype data
    putStrLn "Compiling genotype data"
    runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData showWarnings relevantPackages
        let eigenstratIndEntriesV = V.fromList eigenstratIndEntries
        let newEigenstratIndEntries = [eigenstratIndEntriesV V.! i | i <- indices]
        let jannoIndIds = map posSamIndividualID relevantJannoRows
        when ([n | EigenstratIndEntry n _ _ <-  newEigenstratIndEntries] /= jannoIndIds) $
            throwM (PoseidonValidationException "Cannot forge: order of individuals in genotype indidividual files and Janno-files not consistent")
        let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]    
        runEffect $ eigenstratProd >-> 
            printProgress >->
            P.map (selectIndices indices) >->
            writeEigenstrat outG outS outI newEigenstratIndEntries
        liftIO clearLine
        liftIO $ setCursorColumn 0
        liftIO $ putStrLn "SNPs processed: All done"

selectIndices :: [Int] -> (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine)
selectIndices indices (snpEntry, genoLine) = (snpEntry, V.fromList [genoLine V.! i | i <- indices])

printProgress :: Pipe a a (SafeT IO) ()
printProgress = loop 0
  where
    loop n = do
        when (n `rem` 1000 == 0) $ do
            liftIO clearLine
            liftIO $ setCursorColumn 0
            liftIO $ putStr ("SNPs processed: " ++ show n)
            liftIO $ hFlush stdout
        x <- await
        yield x
        loop (n+1)

findNonExistentEntities :: ForgeRecipe -> [PoseidonPackage] -> IO [ForgeEntity]
findNonExistentEntities entities packages = do
    inds <- concat <$> mapM getIndividuals packages
    let titlesPac     = map posPacTitle packages
        indNamesPac   = [ind   | EigenstratIndEntry ind _ _     <- inds]
        groupNamesPac = [group | EigenstratIndEntry _   _ group <- inds]
    let titlesRequestedPacs = [ pac   | ForgePac   pac   <- entities]
        groupNamesStats     = [ group | ForgeGroup group <- entities]
        indNamesStats       = [ ind   | ForgeInd   ind   <- entities]
    let missingPacs   = map ForgePac $ titlesRequestedPacs \\ titlesPac
        missingInds   = map ForgeInd $ indNamesStats \\ indNamesPac
        missingGroups = map ForgeGroup $ groupNamesStats \\ groupNamesPac
    return $ missingPacs ++ missingInds ++ missingGroups

filterPackages :: ForgeRecipe -> [PoseidonPackage] -> IO [PoseidonPackage]
filterPackages entities packages = do
    let requestedPacs   = [ pac   | ForgePac   pac   <- entities]
        groupNamesStats = [ group | ForgeGroup group <- entities]
        indNamesStats   = [ ind   | ForgeInd   ind   <- entities]
    fmap catMaybes . forM packages $ \pac -> do
        inds <- getIndividuals pac
        let indNamesPac   = [ind   | EigenstratIndEntry ind _ _     <- inds]
            groupNamesPac = [group | EigenstratIndEntry _   _ group <- inds]
        if  posPacTitle pac `elem` requestedPacs
            ||  length (intersect indNamesPac indNamesStats) > 0
            ||  length (intersect groupNamesPac groupNamesStats) > 0
        then return (Just pac)
        else return Nothing

filterJannoRows :: ForgeRecipe -> [PoseidonSample] -> [PoseidonSample]
filterJannoRows entities samples =
    let groupNamesStats = [ group | ForgeGroup group <- entities]
        indNamesStats   = [ ind   | ForgeInd   ind   <- entities]
        comparison x    =  posSamIndividualID x `elem` indNamesStats
                           || head (posSamGroupName x) `elem` groupNamesStats
    in filter comparison samples

filterJannoFiles :: ForgeRecipe -> [(String, [PoseidonSample])] -> [PoseidonSample]
filterJannoFiles entities packages =
    let requestedPacs           = [ pac | ForgePac pac <- entities]
        filterJannoOrNot (a, b) = if a `elem` requestedPacs 
                                  then b
                                  else filterJannoRows entities b
    in concatMap filterJannoOrNot packages

filterBibEntries :: [PoseidonSample] -> [Reference] -> [Reference]
filterBibEntries samples references =
    let relevantPublications = nub $ mapMaybe posSamPublication samples
    in filter (\x-> (unpack . unLiteral . refId) x `elem` relevantPublications) references

extractEntityIndices :: ForgeRecipe -> [PoseidonPackage] -> IO [Int]
extractEntityIndices entities relevantPackages = do
    let pacNames   = [ pac | ForgePac pac <- entities]
        groupNames = [ group | ForgeGroup group <- entities]
        indNames   = [ ind   | ForgeInd   ind   <- entities]
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
