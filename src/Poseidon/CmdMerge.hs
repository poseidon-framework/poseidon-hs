module Poseidon.CmdMerge (runMerge, MergeOptions(..)) where

import           Poseidon.Package           (PoseidonPackage(..),
                                            loadPoseidonPackages,
                                            maybeLoadJannoFiles,
                                            maybeLoadBibTeXFiles,
                                            bibToSimpleMaybeList,
                                            jannoToSimpleMaybeList,
                                            writeJannoFile,
                                            writeBibTeXFile,
                                            PoseidonSample(..),
                                            GenotypeDataSpec(..),
                                            GenotypeFormatSpec(..),
                                            ContributorSpec(..),
                                            getJointGenotypeData)    
import           Control.Monad              (when)
import           Data.Aeson                 (encodeFile)
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

-- | A datatype representing command line options for the survey command
data MergeOptions = MergeOptions
    { _jaBaseDirs  :: [FilePath]
    , _outPacPath  :: FilePath
    , _outPacName  :: String
    }

-- | The main function running the janno command
runMerge :: MergeOptions -> IO ()
runMerge (MergeOptions baseDirs outPath outName) = do
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
    writeJannoFile (outPath </> jannoFile) goodJannoRows
    writeBibTeXFile (outPath </> "LITERATURE.bib") goodBibEntries
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
