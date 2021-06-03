module Poseidon.GoldenTestsRunCommands (
    createStaticCheckSumFile, createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile
    ) where

import           Poseidon.EntitiesList      (PoseidonEntity (..))
import           Poseidon.CLI.Init          (InitOptions (..), runInit)
import           Poseidon.CLI.Fetch         (FetchOptions (..), runFetch)
import           Poseidon.CLI.List          (ListOptions (..), runList, 
                                             RepoLocationSpec (..), ListEntity (..))
import           Poseidon.CLI.Summarise     (SummariseOptions (..), runSummarise)
import           Poseidon.CLI.Survey        (SurveyOptions(..), runSurvey)
--import           Poseidon.CLI.Validate      (ValidateOptions(..), runValidate)
import           Poseidon.GenotypeData      (GenotypeFormatSpec (..), 
                                             SNPSetSpec (..))
import           Poseidon.Package           (getChecksum)

import           Control.Monad              (when, unless)
import           GHC.IO.Handle              (hDuplicateTo, hDuplicate, hClose)
import           System.Directory           (createDirectory, removeDirectoryRecursive, doesDirectoryExist)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (stdout, IOMode(WriteMode), withFile, openFile, stderr, hPutStrLn)

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
    testPipelineInit  testDir checkFilePath testPacsDir
    hPutStrLn stderr "--- list ---"
    testPipelineList  testDir checkFilePath
    hPutStrLn stderr "--- summarise ---"
    testPipelineSummarise testDir checkFilePath
    hPutStrLn stderr "--- survey ---"
    testPipelineSurvey testDir checkFilePath
    hPutStrLn stderr "--- fetch ---"
    testPipelineFetch testDir checkFilePath
    -- hPutStrLn stderr "--- validate ---"
    -- testPipelineValidate testDir checkFilePath
    -- close error sink
    hClose devNull
    unless interactive $ hDuplicateTo stderr_old stderr

testPipelineInit :: FilePath -> FilePath -> FilePath -> IO ()
testPipelineInit testDir checkFilePath testPacsDir = do
    let initOpts1 = InitOptions {
          _inGenoFormat = GenotypeFormatEigenstrat
        , _inGenoSnpSet = SNPSetOther
        , _inGenoFile   = testPacsDir </> "Schiffels_2016" </> "geno.txt"
        , _inSnpFile    = testPacsDir </> "Schiffels_2016" </> "snp.txt"
        , _inIndFile    = testPacsDir </> "Schiffels_2016" </> "ind.txt"
        , _outPacPath   = testDir </> "Schiffels"
        , _outPacName   = "Schiffels"
    }
    runAndChecksumFiles checkFilePath testDir (runInit initOpts1) "init" [
          "Schiffels" </> "POSEIDON.yml"
        , "Schiffels" </> "Schiffels.janno"
        , "Schiffels" </> "geno.txt"
        ]
    let initOpts2 = InitOptions {
          _inGenoFormat = GenotypeFormatPlink 
        , _inGenoSnpSet = SNPSetOther
        , _inGenoFile   = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.bed"
        , _inSnpFile    = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.bim"
        , _inIndFile    = testPacsDir </> "Wang_Plink_test_2020" </> "Wang_2020.fam"
        , _outPacPath   = testDir </> "Wang"
        , _outPacName   = "Wang"
    }
    runAndChecksumFiles checkFilePath testDir (runInit initOpts2) "init" [
          "Wang" </> "POSEIDON.yml"
        , "Wang" </> "Wang.janno"
        , "Wang" </> "Wang_2020.bed"
        ]


-- testPipelineValidate :: FilePath -> FilePath -> IO ()
-- testPipelineValidate testDir checkFilePath = do
--     let validateOpts1 = ValidateOptions {
--           _validateBaseDirs     = [testDir]
--         , _validateVerbose      = False
--         , _validateIgnoreGeno   = False
--     }
--     runAndChecksumStdOut checkFilePath testDir (runValidate validateOpts1) "validate" 1
--     let validateOpts2 = ValidateOptions {
--           _validateBaseDirs     = [testDir]
--         , _validateVerbose      = True
--         , _validateIgnoreGeno   = False
--     }
--     runAndChecksumStdOut checkFilePath testDir (runValidate validateOpts2) "validate" 2
--     let validateOpts3 = ValidateOptions {
--           _validateBaseDirs     = [testDir]
--         , _validateVerbose      = True
--         , _validateIgnoreGeno   = True
--     }
--     runAndChecksumStdOut checkFilePath testDir (runValidate validateOpts3) "validate" 3

testPipelineList :: FilePath -> FilePath -> IO ()
testPipelineList testDir checkFilePath = do
    let listOpts1 = ListOptions {
          _loRepoLocation  = RepoLocal [testDir </> "Schiffels", testDir  </> "Wang"]
        , _loListEntity    = ListPackages
        , _loRawOutput     = False
        , _optIgnoreGeno   = False
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts1) "list" 1
    let listOpts2 = listOpts1 {
          _loListEntity    = ListGroups
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts2) "list" 2
    let listOpts3 = listOpts1 {
          _loListEntity    = ListIndividuals ["Country", "Nr_autosomal_SNPs"]
        }
    runAndChecksumStdOut checkFilePath testDir (runList listOpts3) "list" 3
    let listOpts4 = listOpts3 {
          _loRawOutput     = True
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
