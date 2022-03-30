{-# LANGUAGE OverloadedStrings #-}

module Poseidon.GoldenTestsRunCommands (
    createStaticCheckSumFile, createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile
    ) where

import           Poseidon.EntitiesList          (readEntitiesFromString, PoseidonEntity(..))
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
import           Poseidon.GenotypeData          (InGenotypeData (..),
                                                 GenotypeFormatSpec (..), 
                                                 SNPSetSpec (..))
import           Poseidon.Package               (getChecksum)
import           Poseidon.SecondaryTypes        (ContributorSpec (..),
                                                 VersionComponent (..))

import           Control.Monad                  (when, unless)
import           Data.Either                    (fromRight)
import qualified Data.Text.IO                   as T
import qualified Data.Text                      as T
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
smallTestPacsDir = "test/testDat/testPackages/ancient"
smallTestEntityFiles :: FilePath
smallTestEntityFiles = "test/testDat/testEntityFiles"

createStaticCheckSumFile :: FilePath -> IO ()
createStaticCheckSumFile poseidonHSDir = runCLICommands 
    True
    (poseidonHSDir </> staticTestDir) 
    (poseidonHSDir </> staticCheckSumFile)
    (poseidonHSDir </> smallTestPacsDir)
    (poseidonHSDir </> smallTestEntityFiles)

createDynamicCheckSumFile :: IO ()
createDynamicCheckSumFile = runCLICommands 
    False 
    tempTestDir 
    dynamicCheckSumFile
    smallTestPacsDir
    smallTestEntityFiles

runCLICommands :: Bool -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runCLICommands interactive testDir checkFilePath testPacsDir testEntityFiles = do
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
    testPipelineForge testDir checkFilePath testEntityFiles
    hPutStrLn stderr "--- fetch ---"
    testPipelineFetch testDir checkFilePath
    -- close error sink
    hClose devNull
    unless interactive $ hDuplicateTo stderr_old stderr

testPipelineInit :: FilePath -> FilePath -> FilePath -> IO ()
testPipelineInit testDir checkFilePath testPacsDir = do
    let initOpts1 = InitOptions {
          _initGenoData  = InGenotypeData {
              inFormat   = GenotypeFormatEigenstrat
            , inGenoFile = testPacsDir </> "Schiffels_2016" </> "geno.txt"
            , inSnpFile  = testPacsDir </> "Schiffels_2016" </> "snp.txt"
            , inIndFile  = testPacsDir </> "Schiffels_2016" </> "ind.txt"
            , inSnpSet   = SNPSetOther
            }
        , _initPacPath   = testDir </> "Schiffels"
        , _initPacName   = Just "Schiffels"
        , _initMinimal   = False
    }
    let action = runInit initOpts1 >> patchLastModified testDir ("Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action "init" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "Schiffels.janno"
        , "Schiffels" </> "geno.txt"
        , "Schiffels" </> "Schiffels.bib"
        ]
    let initOpts2 = InitOptions {
          _initGenoData  = InGenotypeData {
              inFormat   = GenotypeFormatPlink
            , inGenoFile = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.bed"
            , inSnpFile  = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.bim"
            , inIndFile  = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.fam"
            , inSnpSet   = SNPSetOther
            }
        , _initPacPath   = testDir </> "Wang"
        , _initPacName   = Nothing
        , _initMinimal   = True
    }
    let action2 = runInit initOpts2 >> patchLastModified testDir ("Wang" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "init" [
          "Wang" </> "POSEIDON.yml"
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
          _listListEntity    = ListIndividuals ["Country", "Nr_SNPs"]
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
        , _genoconvertInGenos = []
        , _genoConvertOutFormat = GenotypeFormatEigenstrat
        , _genoConvertOutOnlyGeno = False
        , _genoconvertRemoveOld = False
    }
    runAndChecksumFiles checkFilePath testDir (runGenoconvert genoconvertOpts1) "genoconvert" [
          "Wang" </> "Wang.geno"
        , "Wang" </> "Wang.snp"
        , "Wang" </> "Wang.ind"
        ]
    let genoconvertOpts2 = GenoconvertOptions {
          _genoconvertBaseDirs = [testDir </> "Schiffels"]
        , _genoconvertInGenos = []
        , _genoConvertOutFormat = GenotypeFormatPlink
        , _genoConvertOutOnlyGeno = False
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
        , _updateIgnorePoseidonVersion = False
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
        , _updateIgnorePoseidonVersion = False
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
        , _updatePoseidonVersion = Nothing
        , _updateIgnorePoseidonVersion = False
        , _updateVersionUpdate = Patch
        , _updateNoChecksumUpdate = False
        , _updateIgnoreGeno = False
        , _updateNewContributors = [
            ContributorSpec "Berta Testfrau" "berta@testfrau.org",
            ContributorSpec "Herbert Testmann" "herbert@testmann.tw"
            ]
        , _updateLog = "test3"
        , _updateForce = True
        }
    let action3 = runUpdate updateOpts3 >> patchLastModified testDir ("Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action3 "update" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "CHANGELOG.md"
        ]

testPipelineForge :: FilePath -> FilePath -> FilePath -> IO ()
testPipelineForge testDir checkFilePath testEntityFiles = do
    -- forge test 1
    let forgeOpts1 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeInGenos      = []
        , _forgeEntitySpec   = Left (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>")
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeFormatEigenstrat
        , _forgeOutMinimal   = False
        , _forgeOutOnlyGeno  = False
        , _forgeOutPacPath   = testDir </> "ForgePac1"
        , _forgeOutPacName   = Just "ForgePac1"
        , _forgeShowWarnings = False
        , _forgeNoExtract    = False
    }
    let action1 = runForge forgeOpts1 >> patchLastModified testDir ("ForgePac1" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action1 "forge" [
          "ForgePac1" </> "POSEIDON.yml"
        , "ForgePac1" </> "ForgePac1.geno"
        , "ForgePac1" </> "ForgePac1.janno"
        ]
    -- forge test 2
    let forgeOpts2 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeInGenos      = []
        , _forgeEntitySpec   = Left (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>,-<SAMPLE3>")
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeFormatPlink
        , _forgeOutMinimal   = True
        , _forgeOutOnlyGeno  = False
        , _forgeOutPacPath   = testDir </> "ForgePac2"
        , _forgeOutPacName   = Nothing
        , _forgeShowWarnings = False
        , _forgeNoExtract    = False
    }
    let action2 = runForge forgeOpts2 >> patchLastModified testDir ("ForgePac2" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "forge" [
          "ForgePac2" </> "POSEIDON.yml"
        , "ForgePac2" </> "ForgePac2.bed"
        ]
    -- forge test 3
    let forgeOpts3 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeInGenos      = []
        , _forgeEntitySpec   = Right (testEntityFiles </> "goldenTestForgeFile1.txt")
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeFormatEigenstrat
        , _forgeOutMinimal   = False
        , _forgeOutOnlyGeno  = False
        , _forgeOutPacPath   = testDir </> "ForgePac3"
        , _forgeOutPacName   = Nothing
        , _forgeShowWarnings = False
        , _forgeNoExtract    = False
    }
    let action3 = runForge forgeOpts3 >> patchLastModified testDir ("ForgePac3" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action3 "forge" [
          "ForgePac3" </> "POSEIDON.yml"
        , "ForgePac3" </> "ForgePac3.geno"
        , "ForgePac3" </> "ForgePac3.snp"
        , "ForgePac3" </> "ForgePac3.ind"
        , "ForgePac3" </> "ForgePac3.janno"
        ]
    -- forge test 4
    let forgeOpts4 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeInGenos      = []
        , _forgeEntitySpec   = Right (testEntityFiles </> "goldenTestForgeFile2.txt")
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeFormatPlink
        , _forgeOutMinimal   = False
        , _forgeOutOnlyGeno  = False
        , _forgeOutPacPath   = testDir </> "ForgePac4"
        , _forgeOutPacName   = Nothing
        , _forgeShowWarnings = False
        , _forgeNoExtract    = False
    }
    let action4 = runForge forgeOpts4 >> patchLastModified testDir ("ForgePac4" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action4 "forge" [
          "ForgePac4" </> "POSEIDON.yml"
        , "ForgePac4" </> "ForgePac4.bim"
        , "ForgePac4" </> "ForgePac4.bed"
        , "ForgePac4" </> "ForgePac4.fam"
        , "ForgePac4" </> "ForgePac4.janno"
        ]
    -- forge test 5
    let forgeOpts5 = ForgeOptions { 
          _forgeBaseDirs     = [testDir </> "Schiffels", testDir </> "Wang"]
        , _forgeInGenos      = []
        , _forgeEntitySpec   = Left []
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeFormatEigenstrat
        , _forgeOutMinimal   = False
        , _forgeOutOnlyGeno  = False
        , _forgeOutPacPath   = testDir </> "ForgePac5"
        , _forgeOutPacName   = Just "ForgePac5"
        , _forgeShowWarnings = False
        , _forgeNoExtract    = False
        , _forgeSnpFile      = Nothing
    }
    let action5 = runForge forgeOpts5 >> patchLastModified testDir ("ForgePac5" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action5 "forge" [
          "ForgePac5" </> "POSEIDON.yml"
        , "ForgePac5" </> "ForgePac5.geno"
        , "ForgePac5" </> "ForgePac5.janno"
        ]

 -- Note: We here use our test server (no SSL and different port). The reason is that 
 -- sometimes we would like to implement new features that affect the communication
 -- between server and client, and we need tests succeeding before Pull Requests are merged, so
 -- we adopt the policy to run experimental builds on the test server in order to test features
 -- before running them on the main server.
testPipelineFetch :: FilePath -> FilePath -> IO ()
testPipelineFetch testDir checkFilePath = do
    let fetchOpts1 = FetchOptions { 
          _jaBaseDirs       = [testDir]
        , _entityList       = [Pac "2019_Nikitin_LBK"]
        , _entityFiles      = []
        , _remoteURL        = "http://c107-224.cloud.gwdg.de:3000"
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
