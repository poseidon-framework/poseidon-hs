{-# LANGUAGE OverloadedStrings #-}

module Poseidon.GoldenTestsRunCommands (
    createStaticCheckSumFile, createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile
    ) where

import           Poseidon.EntitiesList          (PoseidonEntity (..))
import           Poseidon.CLI.Update            (UpdateOptions (..), runUpdate)
import           Poseidon.CLI.Genoconvert       (GenoconvertOptions (..), runGenoconvert)
import           Poseidon.CLI.Init              (InitOptions (..), runInit)
import           Poseidon.CLI.Fetch             (FetchOptions (..), runFetch)
import           Poseidon.CLI.Forge             (ForgeOptions (..), runForge)
import           Poseidon.CLI.List              (ListOptions (..), runList, 
                                                 RepoLocationSpec (..), ListEntity (..))
import           Poseidon.CLI.Summarise         (SummariseOptions (..), runSummarise)
import           Poseidon.CLI.Survey            (SurveyOptions(..), runSurvey)
import           Poseidon.CLI.Validate          (ValidateOptions(..), runValidate)
import           Poseidon.GenotypeData          (GenotypeFormatSpec (..), 
                                                 SNPSetSpec (..))
import           Poseidon.Package               (getChecksum)
import           Poseidon.SecondaryTypes        (ContributorSpec (..),
                                                 VersionComponent (..))

import           Control.Monad                  (when, unless)
import qualified Data.Text.IO                   as T
import qualified Data.Text                      as T
import           Data.Version                   (makeVersion)
import           GHC.IO.Handle                  (hDuplicateTo, hDuplicate, hClose)
import           System.Directory               (createDirectory, removeDirectoryRecursive, doesDirectoryExist)
import           System.FilePath.Posix          ((</>))
import           System.IO                      (stdout, IOMode(WriteMode), withFile, openFile, stderr, hPutStrLn)

tempTestDir :: FilePath
tempTestDir = "/tmp/poseidonHSGoldenTestData"
staticTestDir :: FilePath
staticTestDir = "test/testDat/poseidonHSGoldenTestData"
staticCheckSumFile :: FilePath
staticCheckSumFile = "test/testDat/poseidonHSGoldenTestCheckSumFile.txt"
dynamicCheckSumFile :: FilePath
dynamicCheckSumFile = "/tmp/poseidon_trident_dynamicCheckSumFile.txt"
smallTestPacsDir :: FilePath
smallTestPacsDir = "test/testDat/testModules/ancient" 

createStaticCheckSumFile :: FilePath -> IO ()
createStaticCheckSumFile poseidonHSDir = runCLICommands 
    True
    (poseidonHSDir </> staticTestDir) 
    (poseidonHSDir </> staticCheckSumFile)
    (poseidonHSDir </> smallTestPacsDir)

createDynamicCheckSumFile :: IO ()
createDynamicCheckSumFile = runCLICommands 
    False 
    tempTestDir 
    dynamicCheckSumFile
    smallTestPacsDir

runCLICommands :: Bool -> FilePath -> FilePath -> FilePath -> IO ()
runCLICommands interactive testDir checkFilePath testPacsDir = do
    -- create temp dir for test output
    tmpTestDirExists <- doesDirectoryExist testDir
    when tmpTestDirExists $ removeDirectoryRecursive testDir
    createDirectory testDir
    -- create/overwrite checksum file
    writeFile checkFilePath "Checksums for trident CLI output\n\ 
        \Automatically generated with: poseidon-devtools updateGoldenTests\n\
        \"
    -- create error sink
    devNull <- openFile "/dev/null" WriteMode
    stderr_old <- hDuplicate stderr
    unless interactive $ hDuplicateTo devNull stderr
    -- run CLI pipeline
    hPutStrLn stderr "--- init ---"
    testPipelineInit testDir checkFilePath testPacsDir
    hPutStrLn stderr "--- validate ---"
    testPipelineValidate testDir checkFilePath
    hPutStrLn stderr "--- list ---"
    testPipelineList testDir checkFilePath
    hPutStrLn stderr "--- summarise ---"
    testPipelineSummarise testDir checkFilePath
    hPutStrLn stderr "--- survey ---"
    testPipelineSurvey testDir checkFilePath
    hPutStrLn stderr "--- genoconvert ---"
    testPipelineGenoconvert testDir checkFilePath
    hPutStrLn stderr "--- update ---"
    testPipelineUpdate testDir checkFilePath
    hPutStrLn stderr "--- forge ---"
    testPipelineForge testDir checkFilePath
    hPutStrLn stderr "--- fetch ---"
    testPipelineFetch testDir checkFilePath
    -- close error sink
    hClose devNull
    unless interactive $ hDuplicateTo stderr_old stderr

testPipelineInit :: FilePath -> FilePath -> FilePath -> IO ()
testPipelineInit testDir checkFilePath testPacsDir = do
    let initOpts1 = InitOptions {
          _initGenoFormat = GenotypeFormatEigenstrat
        , _initGenoSnpSet = SNPSetOther
        , _initGenoFile   = testPacsDir </> "Schiffels_2016" </> "geno.txt"
        , _initSnpFile    = testPacsDir </> "Schiffels_2016" </> "snp.txt"
        , _initIndFile    = testPacsDir </> "Schiffels_2016" </> "ind.txt"
        , _initPacPath   = testDir </> "Schiffels"
        , _initPacName   = "Schiffels"
    }
    let action = runInit initOpts1 >> patchLastModified testDir ("Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action "init" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "Schiffels.janno"
        , "Schiffels" </> "geno.txt"
        ]
    let initOpts2 = InitOptions {
          _initGenoFormat = GenotypeFormatPlink 
        , _initGenoSnpSet = SNPSetOther
        , _initGenoFile   = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.bed"
        , _initSnpFile    = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.bim"
        , _initIndFile    = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.fam"
        , _initPacPath   = testDir </> "Wang"
        , _initPacName   = "Wang"
    }
    let action2 = runInit initOpts2 >> patchLastModified testDir ("Wang" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "init" [
          "Wang" </> "POSEIDON.yml"
        , "Wang" </> "Wang.janno"
        , "Wang" </> "Wang_2020.bed"
        ]

patchLastModified :: FilePath -> FilePath -> IO ()
patchLastModified testDir yamlFile = do
    lines_ <- T.lines <$> T.readFile (testDir </> yamlFile)
    let patchedLines = do
            l <- lines_
            if "lastModified" `T.isPrefixOf` l
                then return "lastModified: 1970-01-01"
                else return l
    T.writeFile (testDir </> yamlFile) (T.unlines patchedLines)

testPipelineValidate :: FilePath -> FilePath -> IO ()
testPipelineValidate testDir checkFilePath = do
    let validateOpts1 = ValidateOptions {
          _validateBaseDirs     = [testDir]
        , _validateVerbose      = False
        , _validateIgnoreGeno   = False
        , _validateNoExitCode   = True
    }
    runAndChecksumStdOut checkFilePath testDir (runValidate validateOpts1) "validate" 1
    let validateOpts2 = validateOpts1 {
          _validateVerbose      = True
    }
    runAndChecksumStdOut checkFilePath testDir (runValidate validateOpts2) "validate" 2
    let validateOpts3 = validateOpts2 {
          _validateIgnoreGeno   = True
    }
    runAndChecksumStdOut checkFilePath testDir (runValidate validateOpts3) "validate" 3

testPipelineList :: FilePath -> FilePath -> IO ()
testPipelineList testDir checkFilePath = do
    let listOpts1 = ListOptions {
          _listRepoLocation  = RepoLocal [testDir </> "Schiffels", testDir  </> "Wang"]
        , _listListEntity    = ListPackages
        , _listRawOutput     = False
        , _listIgnoreGeno   = False
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts1) "list" 1
    let listOpts2 = listOpts1 {
          _listListEntity    = ListGroups
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts2) "list" 2
    let listOpts3 = listOpts1 {
          _listListEntity    = ListIndividuals ["Country", "Nr_autosomal_SNPs"]
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts3) "list" 3
    let listOpts4 = listOpts3 {
          _listRawOutput     = True
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts4) "list" 4

testPipelineSummarise :: FilePath -> FilePath -> IO ()
testPipelineSummarise testDir checkFilePath = do
    let summariseOpts1 = SummariseOptions { 
          _summariseBaseDirs = [testDir]
        , _summariseRawOutput = False
    }
    runAndChecksumStdOut checkFilePath testDir (runSummarise summariseOpts1) "summarise" 1
    let summariseOpts2 = SummariseOptions { 
          _summariseBaseDirs = [testDir]
        , _summariseRawOutput = True
    }
    runAndChecksumStdOut checkFilePath testDir (runSummarise summariseOpts2) "summarise" 2

testPipelineSurvey :: FilePath -> FilePath -> IO ()
testPipelineSurvey testDir checkFilePath = do
    let surveyOpts1 = SurveyOptions { 
          _surveyBaseDirs = [testDir]
        , _surveyRawOutput = False
    }
    runAndChecksumStdOut checkFilePath testDir (runSurvey surveyOpts1) "survey" 1
    let surveyOpts2 = SurveyOptions { 
          _surveyBaseDirs = [testDir]
        , _surveyRawOutput = True
    }
    runAndChecksumStdOut checkFilePath testDir (runSurvey surveyOpts2) "survey" 2

testPipelineGenoconvert :: FilePath -> FilePath -> IO ()
testPipelineGenoconvert testDir checkFilePath = do
    let genoconvertOpts1 = GenoconvertOptions {
          _genoconvertBaseDirs = [testDir </> "Wang"]
        , _genoConvertOutFormat = GenotypeFormatEigenstrat
        , _genoconvertRemoveOld = False
    }
    runAndChecksumFiles checkFilePath testDir (runGenoconvert genoconvertOpts1) "genoconvert" [
          "Wang" </> "Wang.geno"
        , "Wang" </> "Wang.snp"
        , "Wang" </> "Wang.ind"
        ]
    let genoconvertOpts2 = GenoconvertOptions {
          _genoconvertBaseDirs = [testDir </> "Schiffels"]
        , _genoConvertOutFormat = GenotypeFormatPlink
        , _genoconvertRemoveOld = False
    }
    runAndChecksumFiles checkFilePath testDir (runGenoconvert genoconvertOpts2) "genoconvert" [
          "Schiffels" </> "Schiffels.bed"
        , "Schiffels" </> "Schiffels.bim"
        , "Schiffels" </> "Schiffels.fam"
        ]

testPipelineUpdate :: FilePath -> FilePath -> IO ()
testPipelineUpdate testDir checkFilePath = do
    let updateOpts1 = UpdateOptions {
          _updateBaseDirs = [testDir </> "Schiffels"]
        , _updatePoseidonVersion = Nothing
        , _updateVersionUpdate = Major
        , _updateNoChecksumUpdate = True
        , _updateIgnoreGeno = True
        , _updateNewContributors = []
        , _updateLog = "test1"
        , _updateForce = True
        }
    let action1 = runUpdate updateOpts1 >> patchLastModified testDir ("Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action1 "update" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "CHANGELOG.md"
        ]
    let updateOpts2 = UpdateOptions {
          _updateBaseDirs = [testDir </> "Schiffels"]
        , _updatePoseidonVersion = Nothing
        , _updateVersionUpdate = Minor
        , _updateNoChecksumUpdate = False
        , _updateIgnoreGeno = False
        , _updateNewContributors = []
        , _updateLog = "test2"
        , _updateForce = False
        }
    let action2 = runUpdate updateOpts2 >> patchLastModified testDir ("Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "update" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "CHANGELOG.md"
        ]
    let updateOpts3 = UpdateOptions {
          _updateBaseDirs = [testDir </> "Schiffels"]
        , _updatePoseidonVersion = Just $ makeVersion [2,9,9]
        , _updateVersionUpdate = Patch
        , _updateNoChecksumUpdate = False
        , _updateIgnoreGeno = False
        , _updateNewContributors = [
            ContributorSpec "Berta Testfrau" "berta@testfrau.org",
            ContributorSpec "Herbert Testmann" "herbert@testmann.tw"
            ]
        , _updateLog = "test3"
        , _updateForce = False
        }
    let action3 = runUpdate updateOpts3 >> patchLastModified testDir ("Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action3 "update" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "CHANGELOG.md"
        ]

testPipelineForge :: FilePath -> FilePath -> IO ()
testPipelineForge testDir checkFilePath = do
    let forgeOpts1 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeEntityList   = [Group "POP2", Ind "SAMPLE2", Ind "SAMPLE4"]
        , _forgeEntityFiles  = []
        , _forgeIntersect    = False
        , _forgeOutPacPath   = testDir </> "ForgePac1"
        , _forgeOutPacName   = "ForgePac1"
        , _forgeOutFormat    = GenotypeFormatEigenstrat
        , _forgeShowWarnings = False
    }
    let action1 = runForge forgeOpts1 >> patchLastModified testDir ("ForgePac1" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action1 "forge" [
          "ForgePac1" </> "POSEIDON.yml"
        , "ForgePac1" </> "ForgePac1.geno"
        , "ForgePac1" </> "ForgePac1.janno"
        ]
    let forgeOpts2 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeEntityList   = [Group "POP2", Ind "SAMPLE2", Ind "SAMPLE4"]
        , _forgeEntityFiles  = []
        , _forgeIntersect    = False
        , _forgeOutPacPath   = testDir </> "ForgePac2"
        , _forgeOutPacName   = "ForgePac2"
        , _forgeOutFormat    = GenotypeFormatPlink
        , _forgeShowWarnings = False
    }
    let action2 = runForge forgeOpts2 >> patchLastModified testDir ("ForgePac2" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "forge" [
          "ForgePac2" </> "POSEIDON.yml"
        , "ForgePac2" </> "ForgePac2.bed"
        , "ForgePac2" </> "ForgePac2.janno"
        ]

testPipelineFetch :: FilePath -> FilePath -> IO ()
testPipelineFetch testDir checkFilePath = do
    let fetchOpts1 = FetchOptions { 
          _jaBaseDirs       = [testDir]
        , _entityList       = [Pac "2019_Nikitin_LBK"]
        , _entityFiles      = []
        , _remoteURL        = "https://c107-224.cloud.gwdg.de"
        , _upgrade          = True
        , _downloadAllPacs  = False 
        }
    runAndChecksumFiles checkFilePath testDir (runFetch fetchOpts1) "fetch" [
          "2019_Nikitin_LBK" </> "POSEIDON.yml"
        , "2019_Nikitin_LBK" </> "Nikitin_LBK.janno"
        , "2019_Nikitin_LBK" </> "Nikitin_LBK.fam"
        ]

runAndChecksumFiles :: FilePath -> FilePath -> IO () -> String -> [FilePath] -> IO ()
runAndChecksumFiles checkSumFilePath testDir action actionName outFiles = do
    -- run action
    action
    -- append checksums to checksum file
    mapM_ (appendChecksum checkSumFilePath testDir actionName) outFiles
    where
        appendChecksum :: FilePath -> FilePath -> FilePath -> String -> IO ()
        appendChecksum checkSumFilePath_ testDir_ actionName_ outFile = do
            checksum <- getChecksum $ testDir_ </> outFile
            appendFile checkSumFilePath_ $ "\n" ++ checksum ++ " " ++ actionName_ ++ " " ++ outFile

runAndChecksumStdOut :: FilePath -> FilePath -> IO () -> String -> Integer -> IO ()
runAndChecksumStdOut checkSumFilePath testDir action actionName outFileNumber = do
    -- store stdout in a specific output file
    let outFile = actionName ++ show outFileNumber
    withFile (testDir </> outFile) WriteMode $ \handle -> do
        -- backup stdout handle
        stdout_old <- hDuplicate stdout
        -- redirect stdout to file
        hDuplicateTo handle stdout
        -- run action
        action
        -- load backup again
        hDuplicateTo stdout_old stdout
    -- append checksum to checksumfile
    checksum <- getChecksum $ testDir </> outFile
    appendFile checkSumFilePath $ "\n" ++ checksum ++ " " ++ actionName ++ " " ++ outFile
