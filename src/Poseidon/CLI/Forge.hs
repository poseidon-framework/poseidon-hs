module Poseidon.CLI.Forge (
    runForge, ForgeOptions(..), ForgeEntity(..), forgeEntitiesParser
    ) where

import           Poseidon.BibFile           (bibToSimpleMaybeList,
                                             writeBibTeXFile)
import           Poseidon.GenotypeData      (GenotypeDataSpec (..), GenotypeFormatSpec (..))
import           Poseidon.Janno             (jannoToSimpleMaybeList,
                                             PoseidonSample (..),
                                             writeJannoFile)
import           Poseidon.Package           (ContributorSpec (..),
                                             PoseidonPackage (..),
                                             getIndividuals,
                                             getJointGenotypeData,
                                             loadPoseidonPackages,
                                             maybeLoadBibTeXFiles,
                                             maybeLoadJannoFiles)
import           Poseidon.Utils             (PoseidonException(..),
                                             renderPoseidonException)

import           Control.Applicative        ((<|>))
import           Control.Exception          (throwIO, try)
import           Control.Monad              (when, forM)
import           Data.Yaml                  (encodeFile)
import           Data.Char                  (isSpace)
import           Data.List                  (nub, sortOn, intersect)
import           Data.Maybe                 (catMaybes, isJust, mapMaybe)
import           Data.Text                  (unpack)
import           Data.Time                  (UTCTime (..), getCurrentTime)
import qualified Data.Vector as V
import           Data.Version               (makeVersion)
import           Pipes                      (runEffect, (>->))
import qualified Pipes.Prelude as P
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat, EigenstratIndEntry (..), EigenstratSnpEntry(..), GenoLine)
import           System.Directory           (createDirectory)
import           System.FilePath            ((<.>), (</>))
import           System.IO                  (hPutStrLn, stderr)
import           Text.CSL.Reference         (refId, unLiteral, Reference (..))
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

-- | A datatype representing command line options for the survey command
data ForgeOptions = ForgeOptions
    { _jaBaseDirs :: [FilePath]
    , _entityList :: [ForgeEntity]
    , _entityFile :: Maybe FilePath
    , _outPacPath :: FilePath
    , _outPacName :: String
    }

-- | A datatype to represent a package, a group or an individual
data ForgeEntity = ForgePac String
    | ForgeGroup String
    | ForgeInd String
    deriving (Eq)

instance Show ForgeEntity where
    show (ForgePac   n) = "*" ++ n ++ "*"
    show (ForgeGroup n) = n
    show (ForgeInd   n) = "<" ++ n ++ ">"

type ForgeRecipe = [ForgeEntity]

-- | A parser to parse forge entities
forgeEntitiesParser :: P.Parser ForgeRecipe
forgeEntitiesParser = P.try (P.sepBy parseForgeEntity (P.char ',' <* P.spaces))

parseForgeEntity :: P.Parser ForgeEntity
parseForgeEntity = parsePac <|> parseGroup <|> parseInd
  where
    parsePac   = ForgePac   <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = ForgeGroup <$> parseName
    parseInd   = ForgeInd   <$> P.between (P.char '<') (P.char '>') parseName
    parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

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
    let groupNames = [ group | ForgeGroup group <- entities]
        indNames   = [ ind   | ForgeInd   ind   <- entities]
    allIndEntries <- fmap concat . mapM getIndividuals $ relevantPackages
    let filterFunc (_, EigenstratIndEntry ind _ group) = ind `elem` indNames || group `elem` groupNames 
    return . map fst . filter filterFunc . zip [0..] $ allIndEntries

readEntitiesFromFile :: FilePath -> IO ForgeRecipe
readEntitiesFromFile entitiesFile = do
    let multiEntityParser = forgeEntitiesParser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- P.parseFromFile (P.spaces *> multiEntityParser <* P.spaces) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonForgeEntityParsingException (show err))
        Right r -> return (concat r)

-- | The main function running the forge command
runForge :: ForgeOptions -> IO ()
runForge (ForgeOptions baseDirs entitiesDirect entitiesFile outPath outName) = do
    -- compile entities
    entitiesFromFile <- case entitiesFile of
        Nothing -> return []
        Just f -> readEntitiesFromFile f
    let entities = entitiesDirect ++ entitiesFromFile
    -- load packages --
    allPackages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ allPackages) ++ " Poseidon packages found"
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
    createDirectory outPath
    let outInd = outName <.> "eigenstrat.ind"
        outSnp = outName <.> "eigenstrat.snp"
        outGeno = outName <.> "eigenstrat.geno"
        genotypeData = GenotypeDataSpec GenotypeFormatEigenstrat outGeno outSnp outInd
        outJanno = outName <.> "janno"
    -- POSEIDON.yml
    pac <- newPackageTemplate outName genotypeData outJanno
    encodeFile (outPath </> "POSEIDON.yml") pac
    -- janno
    writeJannoFile (outPath </> outJanno) relevantJannoRows
    -- bib
    writeBibTeXFile (outPath </> "LITERATURE.bib") relevantBibEntries
    -- genotype data
    runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData relevantPackages
        let eigenstratIndEntriesV = V.fromList eigenstratIndEntries
        let [outG, outS, outI] = map (outPath </>) [outGeno, outSnp, outInd]    
        runEffect $ eigenstratProd >-> P.map (selectIndices indices) >->
            writeEigenstrat outG outS outI [eigenstratIndEntriesV V.! i | i <- indices]

selectIndices :: [Int] -> (EigenstratSnpEntry, GenoLine) -> (EigenstratSnpEntry, GenoLine)
selectIndices indices (snpEntry, genoLine) = (snpEntry, V.fromList [genoLine V.! i | i <- indices])

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

