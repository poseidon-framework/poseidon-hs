module Poseidon.CLI.Merge (
    runMerge, MergeOptions(..), ForgeEntity(..), forgeEntitiesParser
    ) where

import           Poseidon.BibFile           (bibToSimpleMaybeList,
                                             writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), GenotypeFormatSpec (..))
import           Poseidon.Janno             (jannoToSimpleMaybeList,
                                             writeJannoFile)
import           Poseidon.Package           (ContributorSpec (..),
                                             PoseidonPackage (..),
                                             getIndividuals,
                                             getJointGenotypeData,
                                             loadPoseidonPackages,
                                             maybeLoadBibTeXFiles,
                                             maybeLoadJannoFiles)

import           Control.Applicative        ((<|>))
import           Control.Monad              (when, forM)
import           Data.Aeson                 (encodeFile)
import           Data.Char                  (isSpace)
import           Data.List                  (nub, sortOn, intersect)
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Time                  (UTCTime (..), getCurrentTime)
import           Data.Version               (makeVersion)
import           Pipes                      (runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat, EigenstratIndEntry (..))
import           System.Directory           (createDirectory)
import           System.FilePath            ((<.>), (</>))
import           System.IO                  (hPutStrLn, stderr)
import           Text.CSL.Reference         (refId)
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

-- | A datatype representing command line options for the survey command
data MergeOptions = MergeOptions
    { _jaBaseDirs :: [FilePath]
    , _entityList :: [ForgeEntity]
    , _outPacPath :: FilePath
    , _outPacName :: String
    }

-- | A datatype to represent a package, a group or an individual
data ForgeEntity = ForgePac String
    | ForgeGroup String
    | ForgeInd String
    deriving (Eq)

instance Show ForgeEntity where
    show (ForgePac n) = "*" ++ n ++ "*"
    show (ForgeGroup n) = n
    show (ForgeInd   n) = "<" ++ n ++ ">"

-- | A parser to parse forge entities
forgeEntitiesParser :: P.Parser [ForgeEntity]
forgeEntitiesParser = P.try (P.sepBy parseForgeEntity (P.char ',' <* P.spaces))

parseForgeEntity :: P.Parser ForgeEntity
parseForgeEntity = parsePac <|> parseGroup <|> parseInd
  where
    parsePac = ForgePac <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = ForgeGroup <$> parseName
    parseInd = ForgeInd <$> P.between (P.char '<') (P.char '>') parseName
    parseName = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

findRelevantPackages :: [ForgeEntity] -> [PoseidonPackage] -> IO [PoseidonPackage]
findRelevantPackages entities packages = do
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

-- | The main function running the janno command
runMerge :: MergeOptions -> IO ()
runMerge (MergeOptions baseDirs entities outPath outName) = do
    -- get requested entities
    let requestedPacs   = [ pac   | ForgePac   pac   <- entities]
        requestedGroups = [ group | ForgeGroup group <- entities]
        requestedInds   = [ ind   | ForgeInd   ind   <- entities]
    mapM_ putStrLn requestedPacs
    mapM_ putStrLn requestedGroups
    mapM_ putStrLn requestedInds
    -- load packages
    allPackages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ allPackages) ++ " Poseidon packages found"
    packages <- findRelevantPackages entities allPackages
    putStrLn $ (show . length $ packages) ++ " packages contain data for this forging operation"
    -- collect data
    -- JANNO
    jannoFiles <- maybeLoadJannoFiles packages
    let jannoMaybeList = jannoToSimpleMaybeList jannoFiles
    let anyJannoIssues = not $ all isJust jannoMaybeList
    let goodJannoRows = concat $ catMaybes jannoMaybeList
    -- bib
    bibFiles <- maybeLoadBibTeXFiles packages
    let bibMaybeList = bibToSimpleMaybeList bibFiles
    let anyBibIssues = not $ all isJust bibMaybeList
    let goodBibEntries = nub $ sortOn (show . refId) $ concat $ catMaybes bibMaybeList
    -- create new package
    createDirectory outPath
    let jannoFile = outName <.> "janno"
    --writeJannoFile (outPath </> jannoFile) goodJannoRows
    --writeBibTeXFile (outPath </> "LITERATURE.bib") goodBibEntries
    -- combine genotype data
    let outInd = outName <.> "eigenstrat.ind"
        outSnp = outName <.> "eigenstrat.snp"
        outGeno = outName <.> "eigenstrat.geno"
    runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData packages
        let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]
        runEffect $ eigenstratProd >-> writeEigenstrat outG outS outI eigenstratIndEntries
    let genotypeData = GenotypeDataSpec GenotypeFormatEigenstrat outGeno outSnp outInd
    -- print read issue warning
    when (anyJannoIssues || anyBibIssues) $
        putStrLn "\nThere were issues with incomplete, missing or invalid data. Run trident validate to learn more."
    pac <- newPackageTemplate outName genotypeData jannoFile
    encodeFile (outPath </> "POSEIDON.yml") pac

newPackageTemplate :: String -> GenotypeDataSpec -> FilePath -> IO PoseidonPackage
newPackageTemplate n gd janno = do
    (UTCTime today _) <- getCurrentTime
    return PoseidonPackage {
        posPacPoseidonVersion = makeVersion [2, 0, 1],
        posPacTitle = n,
        posPacDescription = Just "Empty package template. Please add a description",
        posPacContributor = [ContributorSpec "John Doe" "john@doe.net"],
        posPacLastModified = Just today,
        posPacBibFile = Just "LITERATURE.bib",
        posPacGenotypeData = gd,
        posPacJannoFile = Just janno
    }
