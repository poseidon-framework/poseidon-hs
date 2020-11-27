module Poseidon.CLI.Merge (
    runMerge, MergeOptions(..), ForgeEntity(..), forgeEntitiesParser
    ) where

import           Poseidon.Package           (PoseidonPackage(..),
                                            loadPoseidonPackages,
                                            GenotypeDataSpec(..),
                                            GenotypeFormatSpec(..),
                                            ContributorSpec(..),
                                            getJointGenotypeData, maybeLoadJannoFiles, maybeLoadBibTeXFiles) 
import           Poseidon.Janno             (jannoToSimpleMaybeList,
                                            writeJannoFile)
import           Poseidon.BibFile           (bibToSimpleMaybeList,
                                            writeBibTeXFile)

import           Control.Applicative        ((<|>))
import           Control.Monad              (when)
import           Data.Aeson                 (encodeFile)
import           Data.Char                  (isSpace)
import           Data.List                  (nub, sortOn)
import           Data.Maybe                 (catMaybes, isJust)
import           Data.Time                  (UTCTime(..), getCurrentTime)
import           Data.Version               (makeVersion)
import           Pipes                      (runEffect, (>->))
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (writeEigenstrat)
import           System.IO                  (hPutStrLn, stderr)
import           System.FilePath            ((</>), (<.>))
import           System.Directory           (createDirectory)
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

-- | The main function running the janno command
runMerge :: MergeOptions -> IO ()
runMerge (MergeOptions baseDirs entities outPath outName) = do
    -- get requested entities
    let requestedPacs = [ show x | x@ForgePac {} <- entities]
    let requestedGroups = [ show x | x@ForgeGroup {} <- entities]
    let requestedInds = [ show x | x@ForgeInd {} <- entities]
    mapM_ putStrLn requestedPacs
    mapM_ putStrLn requestedGroups
    mapM_ putStrLn requestedInds
    -- load packages
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
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
