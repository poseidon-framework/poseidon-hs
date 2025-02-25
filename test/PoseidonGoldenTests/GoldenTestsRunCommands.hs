{-# LANGUAGE OverloadedStrings #-}

module PoseidonGoldenTests.GoldenTestsRunCommands (
    createStaticCheckSumFile, createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile
    ) where

import           Poseidon.CLI.Fetch         (FetchOptions (..), runFetch)
import           Poseidon.CLI.Forge         (ForgeOptions (..),
                                             ForgeOutMode (GenoOut, MinimalOut, NormalOut, PreservePymlOut),
                                             runForge)
import           Poseidon.CLI.Genoconvert   (GenoconvertOptions (..),
                                             runGenoconvert)
import           Poseidon.CLI.Init          (InitOptions (..), runInit)
import           Poseidon.CLI.Jannocoalesce (CoalesceJannoColumnSpec (..),
                                             JannoCoalesceOptions (..),
                                             JannoSourceSpec (..),
                                             runJannocoalesce)
import           Poseidon.CLI.List          (ListEntity (..), ListOptions (..),
                                             RepoLocationSpec (..), runList)
import           Poseidon.CLI.Rectify       (ChecksumsToRectify (..),
                                             PackageVersionUpdate (..),
                                             RectifyOptions (..), runRectify)
import           Poseidon.CLI.Serve         (ArchiveConfig (..),
                                             ArchiveSpec (..),
                                             ServeOptions (..), runServer)
import           Poseidon.CLI.Summarise     (SummariseOptions (..),
                                             runSummarise)
import           Poseidon.CLI.Survey        (SurveyOptions (..), runSurvey)
import           Poseidon.CLI.Timetravel    (TimetravelOptions (..),
                                             runTimetravel)
import           Poseidon.CLI.Validate      (ValidateOptions (..),
                                             ValidatePlan (..), runValidate)
import           Poseidon.Contributor       (ContributorSpec (..))
import           Poseidon.EntityTypes       (EntityInput (..),
                                             PoseidonEntity (..),
                                             readEntitiesFromString)
import           Poseidon.GenotypeData      (GenoDataSource (..),
                                             GenotypeDataSpec (..),
                                             GenotypeFileSpec (..),
                                             GenotypeOutFormatSpec (..),
                                             SNPSetSpec (..))
import           Poseidon.ServerClient      (AddColSpec (..),
                                             ArchiveEndpoint (..))
import           Poseidon.Utils             (LogMode (..), TestMode (..),
                                             getChecksum, testLog,
                                             usePoseidonLogger)
import           Poseidon.Version           (VersionComponent (..))

import           Control.Concurrent         (forkIO, killThread, newEmptyMVar)
import           Control.Concurrent.MVar    (takeMVar)
import           Control.Exception          (finally)
import           Control.Monad              (forM_, unless, when)
import           Data.Either                (fromRight)
import           Data.Function              ((&))
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Version               (makeVersion)
import           GHC.IO.Handle              (hClose, hDuplicate, hDuplicateTo)
import           Poseidon.CLI.Chronicle     (ChronOperation (..),
                                             ChronicleOptions (..),
                                             runChronicle)
import           Poseidon.Contributor       (ORCID (..))
import           Poseidon.EntityTypes       (PacNameAndVersion (..))
import           Poseidon.Utils             (ErrorLength (..))
import           SequenceFormats.Plink      (PlinkPopNameMode (..))
import           System.Directory           (copyFile, createDirectory,
                                             createDirectoryIfMissing,
                                             doesDirectoryExist, listDirectory,
                                             removeDirectoryRecursive)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (IOMode (WriteMode), hPutStrLn,
                                             openFile, stderr, stdout, withFile)
import           System.Process             (callCommand)

-- file paths --

tempTestDir         :: FilePath
tempTestDir         = "/tmp/poseidonHSGoldenTestData"
staticTestDir       :: FilePath
staticTestDir       = "test/PoseidonGoldenTests/GoldenTestData"
staticCheckSumFile  :: FilePath
staticCheckSumFile  = "test/PoseidonGoldenTests/GoldenTestCheckSumFile.txt"
dynamicCheckSumFile :: FilePath
dynamicCheckSumFile = "/tmp/poseidon_trident_dynamicCheckSumFile.txt"
testPacsDir         :: FilePath
testPacsDir         = "test/testDat/testPackages/ancient"
testPacsDirOther    :: FilePath
testPacsDirOther    = "test/testDat/testPackages/other_test_packages"
testEntityFiles     :: FilePath
testEntityFiles     = "test/testDat/testEntityFiles"

-- helper functions --

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive srcDir destDir = do
  createDirectoryIfMissing True destDir
  entries <- listDirectory srcDir
  forM_ entries $ \entry -> do
    let srcPath = srcDir </> entry
        destPath = destDir </> entry
    isDir <- doesDirectoryExist srcPath
    if isDir
    then do copyDirectoryRecursive srcPath destPath
    else do copyFile srcPath destPath

patchLastModified :: FilePath -> FilePath -> IO ()
patchLastModified testDir yamlFile = do
    lines_ <- T.lines <$> T.readFile (testDir </> yamlFile)
    let patchedLines = do
            l <- lines_
            if "lastModified" `T.isPrefixOf` l
                then return "lastModified: 1970-01-01"
                else return l
    T.writeFile (testDir </> yamlFile) (T.unlines patchedLines)

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
    let outFileName = actionName ++ show outFileNumber
        outDirInTestDir = actionName </> outFileName
        outDir = testDir </> actionName
        outPath = outDir </> outFileName
    -- create outdir if it doesn't exist
    outDirExists <- doesDirectoryExist outDir
    unless outDirExists $ createDirectory outDir
    -- catch stdout and write it to the outPath
    writeStdOutToFile outPath action
    -- append checksum to checksumfile
    checksum <- getChecksum $ outPath
    appendFile checkSumFilePath $ "\n" ++ checksum ++ " " ++ actionName ++ " " ++ outDirInTestDir

writeStdOutToFile :: FilePath -> IO () -> IO ()
writeStdOutToFile path action =
    withFile path WriteMode $ \handle -> do
      -- backup stdout handle
      stdout_old <- hDuplicate stdout
      -- redirect stdout to file
      hDuplicateTo handle stdout
      -- run action
      action
      -- load backup again
      hDuplicateTo stdout_old stdout

-- test pipeline --

createStaticCheckSumFile :: FilePath -> IO ()
createStaticCheckSumFile d = runCLICommands True (d </> staticTestDir) (d </> staticCheckSumFile)
createDynamicCheckSumFile :: IO ()
createDynamicCheckSumFile = runCLICommands False tempTestDir dynamicCheckSumFile

runCLICommands :: Bool -> FilePath -> FilePath -> IO ()
runCLICommands interactive testDir checkFilePath = do
    -- create temp dir for test output
    tmpTestDirExists <- doesDirectoryExist testDir
    when tmpTestDirExists $ removeDirectoryRecursive testDir
    createDirectory testDir
    -- create/overwrite checksum file
    writeFile checkFilePath "Checksums for trident CLI output\n\
        \Automatically generated with: poseidon-devtools golden\n"
    -- create error sink
    devNull <- openFile "/dev/null" WriteMode
    stderr_old <- hDuplicate stderr
    unless interactive $ hDuplicateTo devNull stderr
    -- run CLI pipeline
    hPutStrLn stderr "--* local tests"
    hPutStrLn stderr "--- init"
    testPipelineInit testDir checkFilePath
    hPutStrLn stderr "--- validate"
    testPipelineValidate testDir checkFilePath
    hPutStrLn stderr "--- list"
    testPipelineList testDir checkFilePath
    hPutStrLn stderr "--- summarise"
    testPipelineSummarise testDir checkFilePath
    hPutStrLn stderr "--- survey"
    testPipelineSurvey testDir checkFilePath
    hPutStrLn stderr "--- genoconvert"
    testPipelineGenoconvert testDir checkFilePath
    hPutStrLn stderr "--- rectify"
    testPipelineRectify testDir checkFilePath
    hPutStrLn stderr "--- forge"
    testPipelineForge testDir checkFilePath
    hPutStrLn stderr "--- chronicle & timetravel"
    testPipelineChronicleAndTimetravel testDir checkFilePath
    hPutStrLn stderr "--* test server interaction"
    hPutStrLn stderr "--- fetch"
    testPipelineFetch testDir checkFilePath
    hPutStrLn stderr "--- list --remote"
    testPipelineListRemote testDir checkFilePath
    hPutStrLn stderr "--- jannocoalesce"
    testPipelineJannocoalesce testDir checkFilePath
    -- close error sink
    hClose devNull
    unless interactive $ hDuplicateTo stderr_old stderr

testPipelineInit :: FilePath -> FilePath -> IO ()
testPipelineInit testDir checkFilePath = do
    let initOpts1 = InitOptions {
          _initGenoData  = GenotypeDataSpec {
              genotypeFileSpec = GenotypeEigenstrat {
                _esGenoFile = testPacsDir </> "Schiffels_2016" </> "geno.txt"
              , _esGenoFileChkSum = Nothing
              , _esSnpFile  = testPacsDir </> "Schiffels_2016" </> "snp.txt"
              , _esSnpFileChkSum = Nothing
              , _esIndFile  = testPacsDir </> "Schiffels_2016" </> "ind.txt"
              , _esIndFileChkSum = Nothing
              }
            , genotypeSnpSet   = Just SNPSetOther
            }
        , _initPacPath      = testDir </> "init" </> "Schiffels"
        , _initPacName      = Just "Schiffels"
        , _initMinimal      = False
    }
    let action = testLog (runInit initOpts1) >>
                 patchLastModified testDir ("init" </> "Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action "init" [
          "init" </> "Schiffels" </> "POSEIDON.yml"
        , "init" </> "Schiffels" </> "Schiffels.janno"
        , "init" </> "Schiffels" </> "geno.txt"
        , "init" </> "Schiffels" </> "Schiffels.bib"
        ]

    let initOpts2 = InitOptions {
          _initGenoData  = GenotypeDataSpec {
            genotypeFileSpec = GenotypePlink {
              _plGenoFile = testPacsDir </> "Wang_2020" </> "Wang_2020.bed"
            , _plGenoFileChkSum = Nothing
            , _plSnpFile  = testPacsDir </> "Wang_2020" </> "Wang_2020.bim"
            , _plSnpFileChkSum = Nothing
            , _plIndFile  = testPacsDir </> "Wang_2020" </> "Wang_2020.fam"
            , _plIndFileChkSum = Nothing
            }
          , genotypeSnpSet   = Just SNPSetOther
          }
        , _initPacPath   = testDir </> "init" </> "Wang"
        , _initPacName   = Nothing
        , _initMinimal   = True
    }
    let action2 = testLog (runInit initOpts2) >>
                  patchLastModified testDir ("init" </> "Wang" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "init" [
          "init" </> "Wang" </> "POSEIDON.yml"
        , "init" </> "Wang" </> "Wang_2020.bed"
        ]

    let initOpts3 = InitOptions {
          _initGenoData  = GenotypeDataSpec {
              genotypeFileSpec = GenotypeVCF {
                _vcfGenoFile = testPacsDirOther </> "Schiffels_2016_vcf" </> "geno.vcf"
              , _vcfGenoFileChkSum = Nothing
              }
            , genotypeSnpSet   = Just SNPSetOther
            }
        , _initPacPath      = testDir </> "init_vcf" </> "Schiffels_vcf"
        , _initPacName      = Just "Schiffels"
        , _initMinimal      = False
    }
    let action3 = testLog (runInit initOpts3) >>
                 patchLastModified testDir ("init_vcf" </> "Schiffels_vcf" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action3 "init" [
          "init_vcf" </> "Schiffels_vcf" </> "POSEIDON.yml"
        , "init_vcf" </> "Schiffels_vcf" </> "Schiffels.janno"
        , "init_vcf" </> "Schiffels_vcf" </> "geno.vcf"
        , "init_vcf" </> "Schiffels_vcf" </> "Schiffels.bib"
        ]

testPipelineValidate :: FilePath -> FilePath -> IO ()
testPipelineValidate testDir checkFilePath = do
    let validateOpts1 = ValidateOptions {
          _validatePlan = ValPlanBaseDirs {
              _valPlanBaseDirs         = [testPacsDir]
            , _valPlanIgnoreGeno       = False
            , _valPlanFullGeno         = False
            , _valPlanIgnoreDuplicates = True
            , _valPlanIgnoreChecksums  = False
            , _valPlanIgnorePosVersion = False
            }
        , _validateNoExitCode          = True
        , _validateOnlyLatest          = False
    }
    run 1 validateOpts1
    validateOpts1 {
          _validatePlan = ValPlanBaseDirs {
              _valPlanBaseDirs         = [testPacsDir]
            , _valPlanIgnoreGeno       = True
            , _valPlanFullGeno         = False
            , _valPlanIgnoreDuplicates = True
            , _valPlanIgnoreChecksums  = False
            , _valPlanIgnorePosVersion = False
            }
    } & run 2
    validateOpts1 {
          _validatePlan = ValPlanBaseDirs {
              _valPlanBaseDirs         = [testPacsDir]
            , _valPlanIgnoreGeno       = False
            , _valPlanFullGeno         = True
            , _valPlanIgnoreDuplicates = True
            , _valPlanIgnoreChecksums  = False
            , _valPlanIgnorePosVersion = False
            }
    } & run 3
    -- validate packages generated with init
    validateOpts1 {
          _validatePlan = ValPlanBaseDirs {
              _valPlanBaseDirs = [testDir </> "init"]
            , _valPlanIgnoreGeno       = False
            , _valPlanFullGeno         = False
            , _valPlanIgnoreDuplicates = True
            , _valPlanIgnoreChecksums  = False
            , _valPlanIgnorePosVersion = False
            }
    } & run 4
    -- validate individual files
    validateOpts1 {
          _validatePlan = ValPlanPoseidonYaml $ testPacsDir </> "Schiffels_2016" </> "POSEIDON.yml"
    } & run 5
    validateOpts1 {
          _validatePlan = ValPlanGeno $ GenotypeDataSpec {
              genotypeFileSpec = GenotypeEigenstrat {
                _esGenoFile       = testPacsDir </> "Schiffels_2016" </> "geno.txt"
              , _esGenoFileChkSum = Nothing
              , _esSnpFile        = testPacsDir </> "Schiffels_2016" </> "snp.txt"
              , _esSnpFileChkSum  = Nothing
              , _esIndFile        = testPacsDir </> "Schiffels_2016" </> "ind.txt"
              , _esIndFileChkSum  = Nothing
              }
            , genotypeSnpSet      = Nothing
            }
    } & run 6
    validateOpts1 {
          _validatePlan = ValPlanJanno $ testPacsDir </> "Schiffels_2016" </> "Schiffels_2016.janno"
    } & run 7
    validateOpts1 {
          _validatePlan = ValPlanSSF $ testPacsDir </> "Schiffels_2016" </> "ena_table.ssf"
    } & run 8
    validateOpts1 {
          _validatePlan = ValPlanBib $ testPacsDir </> "Schiffels_2016" </> "sources.bib"
    } & run 9
    where
        run :: Integer -> ValidateOptions -> IO ()
        run nr opts = runAndChecksumStdOut checkFilePath testDir (testLog $ runValidate opts) "validate" nr

testPipelineList :: FilePath -> FilePath -> IO ()
testPipelineList testDir checkFilePath = do
    let listOpts1 = ListOptions {
          _listRepoLocation  = RepoLocal [testPacsDir </> "Schiffels_2016", testPacsDir  </> "Wang_Wang_2020"]
        , _listListEntity    = ListPackages
        , _listRawOutput     = False
        , _listOnlyLatest    = False
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts1) "list" 1

    let listOpts2 = listOpts1 {
          _listListEntity    = ListGroups
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts2) "list" 2

    let listOpts3 = listOpts1 {
          _listListEntity    = ListIndividuals (AddColList ["Country", "Nr_SNPs"])
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts3) "list" 3

    let listOpts4 = listOpts3 {
          _listRawOutput     = True
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts4) "list" 4

    let listOpts5 = listOpts1 {
          _listOnlyLatest     = True
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts5) "list" 5

    let listOpts6 = listOpts1 {
          _listListEntity    = ListIndividuals AddColAll
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts6) "list" 6

    let listOpts7 = listOpts1 {
            _listListEntity = ListBibliography AddColAll
        }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts7) "list" 7


testPipelineSummarise :: FilePath -> FilePath -> IO ()
testPipelineSummarise testDir checkFilePath = do
    let summariseOpts1 = SummariseOptions {
          _summariseBaseDirs = [testPacsDir]
        , _summariseRawOutput = False
    }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runSummarise summariseOpts1) "summarise" 1
    let summariseOpts2 = SummariseOptions {
          _summariseBaseDirs = [testPacsDir]
        , _summariseRawOutput = True
    }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runSummarise summariseOpts2) "summarise" 2

testPipelineSurvey :: FilePath -> FilePath -> IO ()
testPipelineSurvey testDir checkFilePath = do
    let surveyOpts1 = SurveyOptions {
          _surveyBaseDirs = [testPacsDir]
        , _surveyRawOutput = False
        , _surveyOnlyLatest = False
    }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runSurvey surveyOpts1) "survey" 1
    let surveyOpts2 = SurveyOptions {
          _surveyBaseDirs = [testPacsDir]
        , _surveyRawOutput = True
        , _surveyOnlyLatest = False
    }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runSurvey surveyOpts2) "survey" 2
    let surveyOpts3 = SurveyOptions {
          _surveyBaseDirs = [testPacsDir]
        , _surveyRawOutput = False
        , _surveyOnlyLatest = True
    }
    runAndChecksumStdOut checkFilePath testDir (testLog $ runSurvey surveyOpts3) "survey" 3

testPipelineGenoconvert :: FilePath -> FilePath -> IO ()
testPipelineGenoconvert testDir checkFilePath = do
    let genoconvertOpts1 = GenoconvertOptions {
          _genoconvertGenoSources = [PacBaseDir $ testPacsDir </> "Schiffels_2016"]
        , _genoConvertOutFormat = GenotypeOutFormatPlink
        , _genoMaybeOutPackagePath = Just $ testDir </> "genoconvert" </> "Schiffels"
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = False
    }
    runAndChecksumFiles checkFilePath testDir (testLog $ runGenoconvert genoconvertOpts1) "genoconvert" [
          "genoconvert" </> "Schiffels" </> "Schiffels_2016.bed"
        , "genoconvert" </> "Schiffels" </> "Schiffels_2016.bim"
        , "genoconvert" </> "Schiffels" </> "Schiffels_2016.fam"
        ]

    let genoconvertOpts2 = GenoconvertOptions {
          _genoconvertGenoSources = [PacBaseDir $ testPacsDir </> "Schiffels_2016"]
        , _genoConvertOutFormat = GenotypeOutFormatPlink
        , _genoMaybeOutPackagePath = Just $ testDir </> "genoconvert" </> "Schiffels_otherPlinkEncoding"
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsPhenotype
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = False
    }
    runAndChecksumFiles checkFilePath testDir (testLog $ runGenoconvert genoconvertOpts2) "genoconvert" [
          "genoconvert" </> "Schiffels_otherPlinkEncoding" </> "Schiffels_2016.bed"
        , "genoconvert" </> "Schiffels_otherPlinkEncoding" </> "Schiffels_2016.bim"
        , "genoconvert" </> "Schiffels_otherPlinkEncoding" </> "Schiffels_2016.fam"
        ]

    -- in-place conversion
    let genoconvertOpts3 = GenoconvertOptions {
          _genoconvertGenoSources = [PacBaseDir $ testDir </> "init" </> "Wang"]
        , _genoConvertOutFormat = GenotypeOutFormatEigenstrat
        , _genoMaybeOutPackagePath = Nothing
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = False
    }
    runAndChecksumFiles checkFilePath testDir (testLog $ runGenoconvert genoconvertOpts3) "genoconvert" [
          "init" </> "Wang" </> "Wang.geno"
        , "init" </> "Wang" </> "Wang.snp"
        , "init" </> "Wang" </> "Wang.ind"
        ]

    let genoconvertOpts4 = GenoconvertOptions {
          _genoconvertGenoSources = [
            GenoDirect $
              GenotypeDataSpec {
                  genotypeFileSpec = GenotypeEigenstrat {
                    _esGenoFile = testDir </> "init" </> "Schiffels" </> "geno.txt"
                  , _esGenoFileChkSum = Nothing
                  , _esSnpFile  = testDir </> "init" </> "Schiffels" </> "snp.txt"
                  , _esSnpFileChkSum = Nothing
                  , _esIndFile  = testDir </> "init" </> "Schiffels" </> "ind.txt"
                  , _esIndFileChkSum = Nothing
                }
                , genotypeSnpSet   = Just SNPSetOther
              }
          ]
        , _genoConvertOutFormat = GenotypeOutFormatPlink
        , _genoMaybeOutPackagePath = Nothing
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = False
    }
    runAndChecksumFiles checkFilePath testDir (testLog $ runGenoconvert genoconvertOpts4) "genoconvert" [
          "init" </> "Schiffels" </> "geno.bed"
        , "init" </> "Schiffels" </> "geno.bim"
        , "init" </> "Schiffels" </> "geno.fam"
        ]

    let genoconvertOpts5 = GenoconvertOptions {
          _genoconvertGenoSources = [
            GenoDirect $
              GenotypeDataSpec {
                  genotypeFileSpec = GenotypeVCF {
                    _vcfGenoFile = testDir </> "init_vcf" </> "Schiffels_vcf" </> "geno.vcf"
                  , _vcfGenoFileChkSum = Nothing
                }
                , genotypeSnpSet   = Just SNPSetOther
              }
          ]
        , _genoConvertOutFormat = GenotypeOutFormatPlink
        , _genoMaybeOutPackagePath = Nothing
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = False
    }
    runAndChecksumFiles checkFilePath testDir (testLog $ runGenoconvert genoconvertOpts5) "genoconvert" [
          "init_vcf" </> "Schiffels_vcf" </> "geno.bed"
        , "init_vcf" </> "Schiffels_vcf" </> "geno.bim"
        , "init_vcf" </> "Schiffels_vcf" </> "geno.fam"
        ]

    -- round trip test
    let genoconvertOpts6zipping = GenoconvertOptions {
          _genoconvertGenoSources = [PacBaseDir $ testPacsDir </> "Schiffels_2016"]
        , _genoConvertOutFormat = GenotypeOutFormatPlink
        , _genoMaybeOutPackagePath = Just $ testDir </> "genoconvert" </> "zip_roundtrip"
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = True
    }
    testLog $ runGenoconvert genoconvertOpts6zipping

    let genoconvertOpts6unzipping = GenoconvertOptions {
          _genoconvertGenoSources =
              let gSpec = GenotypePlink (testDir </> "genoconvert" </> "zip_roundtrip" </> "Schiffels_2016.bed.gz") Nothing
                                        (testDir </> "genoconvert" </> "zip_roundtrip" </> "Schiffels_2016.bim.gz") Nothing
                                        (testDir </> "genoconvert" </> "zip_roundtrip" </> "Schiffels_2016.fam") Nothing
              in  [GenoDirect $ GenotypeDataSpec gSpec Nothing]
        , _genoConvertOutFormat = GenotypeOutFormatPlink
        , _genoMaybeOutPackagePath = Nothing
        , _genoconvertRemoveOld = True
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = False
    }
    runAndChecksumFiles checkFilePath testDir (testLog $ runGenoconvert genoconvertOpts6unzipping) "genoconvert" [
          "genoconvert" </> "zip_roundtrip" </> "Schiffels_2016.bed"
        , "genoconvert" </> "zip_roundtrip" </> "Schiffels_2016.bim"
        , "genoconvert" </> "zip_roundtrip" </> "Schiffels_2016.fam"
        ]

    let genoconvertOpts7 = GenoconvertOptions {
          _genoconvertGenoSources = [PacBaseDir $ testPacsDir </> "Schiffels_2016"]
        , _genoConvertOutFormat = GenotypeOutFormatVCF
        , _genoMaybeOutPackagePath = Just $ testDir </> "genoconvert" </> "out_vcf"
        , _genoconvertRemoveOld = False
        , _genoconvertOutPlinkPopMode = PlinkPopNameAsFamily
        , _genoconvertOnlyLatest = False
        , _genoconvertOutZip     = True
    }
    testLog $ runGenoconvert genoconvertOpts7

testPipelineRectify :: FilePath -> FilePath -> IO ()
testPipelineRectify testDir checkFilePath = do
    let rectifyOpts1 = RectifyOptions {
          _rectifyBaseDirs = [testDir </> "init" </> "Schiffels"]
        , _rectifyPoseidonVersion = Nothing
        , _rectifyIgnorePoseidonVersion = False
        , _rectifyPackageVersionUpdate = Just (PackageVersionUpdate Major (Just "test1"))
        , _rectifyChecksums = ChecksumNone
        , _rectifyNewContributors = Nothing
        , _rectifyJannoRemoveEmptyCols = False
        , _rectifyOnlyLatest = False
        }
    let action1 = testLog (runRectify rectifyOpts1) >> patchLastModified testDir ("init" </> "Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action1 "rectify" [
          "init" </> "Schiffels" </> "POSEIDON.yml"
        , "init" </> "Schiffels" </> "CHANGELOG.md"
        ]
    let rectifyOpts2 = RectifyOptions {
          _rectifyBaseDirs = [testDir </> "init" </> "Schiffels"]
        , _rectifyPoseidonVersion = Just $ makeVersion [2,7,1]
        , _rectifyIgnorePoseidonVersion = False
        , _rectifyPackageVersionUpdate = Just (PackageVersionUpdate Minor (Just "test2"))
        , _rectifyChecksums = ChecksumAll
        , _rectifyNewContributors = Nothing
        , _rectifyJannoRemoveEmptyCols = False
        , _rectifyOnlyLatest = False
        }
    let action2 = testLog (runRectify rectifyOpts2) >> patchLastModified testDir ("init" </> "Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "rectify" [
          "init" </> "Schiffels" </> "POSEIDON.yml"
        , "init" </> "Schiffels" </> "CHANGELOG.md"
        ]
    let rectifyOpts3 = RectifyOptions {
          _rectifyBaseDirs = [testDir </> "init" </> "Schiffels"]
        , _rectifyPoseidonVersion = Nothing
        , _rectifyIgnorePoseidonVersion = False
        , _rectifyPackageVersionUpdate = Just (PackageVersionUpdate Patch Nothing)
        , _rectifyChecksums = ChecksumNone
        , _rectifyNewContributors = Just [
              ContributorSpec "Josiah Carberry" "carberry@brown.edu" (Just $ ORCID {_orcidNums = "000000021825009", _orcidChecksum = '7'})
            , ContributorSpec "Herbert Testmann" "herbert@testmann.tw" Nothing
            ]
        , _rectifyJannoRemoveEmptyCols = False
        , _rectifyOnlyLatest = False
        }
    let action3 = testLog (runRectify rectifyOpts3) >> patchLastModified testDir ("init" </> "Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action3 "rectify" [
          "init" </> "Schiffels" </> "POSEIDON.yml"
        , "init" </> "Schiffels" </> "CHANGELOG.md"
        ]
    let rectifyOpts4 = RectifyOptions {
          _rectifyBaseDirs = [testDir </> "init" </> "Schiffels"]
        , _rectifyPoseidonVersion = Nothing
        , _rectifyIgnorePoseidonVersion = False
        , _rectifyPackageVersionUpdate = Nothing
        , _rectifyChecksums = ChecksumAll
        , _rectifyNewContributors = Nothing
        , _rectifyJannoRemoveEmptyCols = True
        , _rectifyOnlyLatest = False
        }
    let action4 = testLog (runRectify rectifyOpts4) >> patchLastModified testDir ("init" </> "Schiffels" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action4 "rectify" [
          "init" </> "Schiffels" </> "POSEIDON.yml"
        , "init" </> "Schiffels" </> "CHANGELOG.md"
        , "init" </> "Schiffels" </> "Schiffels.janno"
        ]

testPipelineForge :: FilePath -> FilePath -> IO ()
testPipelineForge testDir checkFilePath = do
    -- forge test 1
    let forgeOpts1 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Wang_2020"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac1"
        , _forgeOutPacName   = Just "ForgePac1"
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action1 = testLog (runForge forgeOpts1) >> patchLastModified testDir ("forge" </> "ForgePac1" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action1 "forge" [
          "forge" </> "ForgePac1" </> "POSEIDON.yml"
        , "forge" </> "ForgePac1" </> "ForgePac1.geno"
        , "forge" </> "ForgePac1" </> "ForgePac1.janno"
        , "forge" </> "ForgePac1" </> "ForgePac1.ssf"
        , "forge" </> "ForgePac1" </> "ForgePac1.bib"
        ]

    -- forge test 1 with VCF and zip output
    let forgeOpts1vcf = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDirOther </> "Schiffels_2016_vcf", PacBaseDir $ testPacsDir </> "Wang_2020"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac1_vcf"
        , _forgeOutPacName   = Just "ForgePac1_vcf"
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action1vcf = testLog (runForge forgeOpts1vcf) >> patchLastModified testDir ("forge" </> "ForgePac1_vcf" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action1vcf "forge" [
          "forge" </> "ForgePac1_vcf" </> "POSEIDON.yml"
        , "forge" </> "ForgePac1_vcf" </> "ForgePac1_vcf.geno"
        , "forge" </> "ForgePac1_vcf" </> "ForgePac1_vcf.janno"
        , "forge" </> "ForgePac1_vcf" </> "ForgePac1_vcf.ssf"
        , "forge" </> "ForgePac1_vcf" </> "ForgePac1_vcf.bib"
        ]

    -- forge test 2
    let forgeOpts2 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Wang_2020"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>,-<SAMPLE3>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatPlink
        , _forgeOutMode      = MinimalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac2"
        , _forgeOutPacName   = Nothing
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action2 = testLog (runForge forgeOpts2) >> patchLastModified testDir ("forge" </> "ForgePac2" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action2 "forge" [
          "forge" </> "ForgePac2" </> "POSEIDON.yml"
        , "forge" </> "ForgePac2" </> "ForgePac2.bed"
        ]

    -- forge test 3
    let forgeOpts3 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Wang_2020"]
        , _forgeEntityInput  = [EntitiesFromFile (testEntityFiles </> "goldenTestForgeFile1.txt")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac3"
        , _forgeOutPacName   = Nothing
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action3 = testLog (runForge forgeOpts3) >> patchLastModified testDir ("forge" </> "ForgePac3" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action3 "forge" [
          "forge" </> "ForgePac3" </> "POSEIDON.yml"
        , "forge" </> "ForgePac3" </> "ForgePac3.geno"
        , "forge" </> "ForgePac3" </> "ForgePac3.snp"
        , "forge" </> "ForgePac3" </> "ForgePac3.ind"
        , "forge" </> "ForgePac3" </> "ForgePac3.janno"
        , "forge" </> "ForgePac3" </> "ForgePac3.ssf"
        ]

    -- forge test 4
    let forgeOpts4 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Wang_2020"]
        , _forgeEntityInput  = [EntitiesFromFile (testEntityFiles </> "goldenTestForgeFile2.txt")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatPlink
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac4"
        , _forgeOutPacName   = Nothing
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action4 = testLog (runForge forgeOpts4) >> patchLastModified testDir ("forge" </> "ForgePac4" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action4 "forge" [
          "forge" </> "ForgePac4" </> "POSEIDON.yml"
        , "forge" </> "ForgePac4" </> "ForgePac4.bim"
        , "forge" </> "ForgePac4" </> "ForgePac4.bed"
        , "forge" </> "ForgePac4" </> "ForgePac4.fam"
        , "forge" </> "ForgePac4" </> "ForgePac4.janno"
        , "forge" </> "ForgePac4" </> "ForgePac4.ssf"
        ]

    -- forge test 5
    let forgeOpts5 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Wang_2020"]
        , _forgeEntityInput  = []
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac5"
        , _forgeOutPacName   = Just "ForgePac5"
        , _forgePackageWise  = False
        , _forgeSnpFile      = Nothing
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action5 = testLog (runForge forgeOpts5) >> patchLastModified testDir ("forge" </> "ForgePac5" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action5 "forge" [
          "forge" </> "ForgePac5" </> "POSEIDON.yml"
        , "forge" </> "ForgePac5" </> "ForgePac5.geno"
        , "forge" </> "ForgePac5" </> "ForgePac5.janno"
        , "forge" </> "ForgePac5" </> "ForgePac5.ssf"
        ]

    -- forge test 6 (direct genotype data input interface)
    let forgeOpts6 = ForgeOptions {
          _forgeGenoSources = [
            GenoDirect $
              GenotypeDataSpec {
                  genotypeFileSpec = GenotypeEigenstrat {
                    _esGenoFile = testPacsDir </> "Schiffels_2016" </> "geno.txt"
                  , _esGenoFileChkSum = Nothing
                  , _esSnpFile  = testPacsDir </> "Schiffels_2016" </> "snp.txt"
                  , _esSnpFileChkSum = Nothing
                  , _esIndFile  = testPacsDir </> "Schiffels_2016" </> "ind.txt"
                  , _esIndFileChkSum = Nothing
                }
                , genotypeSnpSet   = Just SNPSetOther
              },
            GenoDirect $
              GenotypeDataSpec {
                  genotypeFileSpec = GenotypePlink {
                    _plGenoFile = testPacsDir </> "Wang_2020" </> "Wang_2020.bed"
                  , _plGenoFileChkSum = Nothing
                  , _plSnpFile  = testPacsDir </> "Wang_2020" </> "Wang_2020.bim"
                  , _plSnpFileChkSum = Nothing
                  , _plIndFile  = testPacsDir </> "Wang_2020" </> "Wang_2020.fam"
                  , _plIndFileChkSum = Nothing
                }
                , genotypeSnpSet   = Just SNPSetOther
              }
          ]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>")]
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = GenoOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac6"
        , _forgeOutPacName   = Just "ForgePac6"
        , _forgePackageWise  = False
        , _forgeSnpFile      = Nothing
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action6 = testLog (runForge forgeOpts6)
    runAndChecksumFiles checkFilePath testDir action6 "forge" [
          "forge" </> "ForgePac6" </> "ForgePac6.geno"
        , "forge" </> "ForgePac6" </> "ForgePac6.snp"
        , "forge" </> "ForgePac6" </> "ForgePac6.ind"
        ]

    -- forge test 7 (mixed data input interface)
    let forgeOpts7 = ForgeOptions {
          _forgeGenoSources  = [
            PacBaseDir $ testPacsDir </> "Schiffels_2016",
            GenoDirect $
              GenotypeDataSpec {
                  genotypeFileSpec = GenotypePlink {
                    _plGenoFile = testPacsDir </> "Wang_2020" </> "Wang_2020.bed"
                  , _plGenoFileChkSum = Nothing
                  , _plSnpFile  = testPacsDir </> "Wang_2020" </> "Wang_2020.bim"
                  , _plSnpFileChkSum = Nothing
                  , _plIndFile  = testPacsDir </> "Wang_2020" </> "Wang_2020.fam"
                  , _plIndFileChkSum = Nothing
                }
                , genotypeSnpSet   = Just SNPSetOther
              }
            ]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP2,<SAMPLE2>,<SAMPLE4>")]
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac7"
        , _forgeOutPacName   = Just "ForgePac7"
        , _forgePackageWise  = False
        , _forgeSnpFile      = Nothing
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action7 = testLog (runForge forgeOpts7) >> patchLastModified testDir ("forge" </> "ForgePac7" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action7 "forge" [
          "forge" </> "ForgePac7" </> "ForgePac7.geno"
        , "forge" </> "ForgePac7" </> "ForgePac7.snp"
        , "forge" </> "ForgePac7" </> "ForgePac7.ind"
        , "forge" </> "ForgePac7" </> "ForgePac7.janno"
        ]

    -- forge test 8 (combining additional janno columns from separate source janno files)
    let forgeOpts8 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Lamnidis_2018_newVersion"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "<XXX001>,<XXX011>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac8"
        , _forgeOutPacName   = Just "ForgePac8"
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action8 = testLog (runForge forgeOpts8) >> patchLastModified testDir ("forge" </> "ForgePac8" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action8 "forge" [
          "forge" </> "ForgePac8" </> "ForgePac8.janno"
        , "forge" </> "ForgePac8" </> "ForgePac8.ssf"
        ]

    -- forge test 9 (duplicates are handled correctly if an individual is properly specified)
    let forgeOpts9 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Schmid_2028"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP1,-<Schiffels_2016:POP1:XXX001>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac9"
        , _forgeOutPacName   = Just "ForgePac9"
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action9 = testLog (runForge forgeOpts9) >> patchLastModified testDir ("forge" </> "ForgePac9" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action9 "forge" [
          "forge" </> "ForgePac9" </> "ForgePac9.geno"
        , "forge" </> "ForgePac9" </> "ForgePac9.janno"
        , "forge" </> "ForgePac9" </> "ForgePac9.ssf"
        ]

    -- forge test 10 (duplicates can also be resolved with negative selection)
    let forgeOpts10 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Schmid_2028"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "-<Schmid_2028:POP1:XXX001>,-<Schiffels_2016:POP2:XXX002>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac10"
        , _forgeOutPacName   = Just "ForgePac10"
        , _forgePackageWise    = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action10 = testLog (runForge forgeOpts10) >> patchLastModified testDir ("forge" </> "ForgePac10" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action10 "forge" [
          "forge" </> "ForgePac10" </> "ForgePac10.geno"
        , "forge" </> "ForgePac10" </> "ForgePac10.janno"
        , "forge" </> "ForgePac10" </> "ForgePac10.ssf"
        , "forge" </> "ForgePac10" </> "ForgePac10.bib"
        ]

    -- forge test 11 (--packagewise works as expected)
    let forgeOpts11 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016", PacBaseDir $ testPacsDir </> "Schmid_2028"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP3")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac11"
        , _forgeOutPacName   = Just "ForgePac11"
        , _forgePackageWise  = True
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action11 = testLog (runForge forgeOpts11) >> patchLastModified testDir ("forge" </> "ForgePac11" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action11 "forge" [
          "forge" </> "ForgePac11" </> "ForgePac11.geno"
        , "forge" </> "ForgePac11" </> "ForgePac11.janno"
        , "forge" </> "ForgePac11" </> "ForgePac11.ssf"
        ]

    -- simple package version selection
    let forgeOpts12 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "*Lamnidis_2018-1.0.0*")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac12"
        , _forgeOutPacName   = Just "ForgePac12"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action12 = testLog (runForge forgeOpts12) >> patchLastModified testDir ("forge" </> "ForgePac12" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action12 "forge" [
          "forge" </> "ForgePac12" </> "ForgePac12.ind"
        ]

    -- merge an explicitly versioned package with another package
    let forgeOpts13 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "*Lamnidis_2018-1.0.1*,*Schiffels_2016*")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac13"
        , _forgeOutPacName   = Just "ForgePac13"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action13 = testLog (runForge forgeOpts13) >> patchLastModified testDir ("forge" </> "ForgePac13" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action13 "forge" [
          "forge" </> "ForgePac13" </> "ForgePac13.janno"
        ]

    -- use the SpecificInd interface to merge individuals from the same package across different versions
    let forgeOpts14 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $
            readEntitiesFromString "<Lamnidis_2018-1.0.1:POP1:XXX017>,<Lamnidis_2018-1.0.0:POP3:XXX018>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac14"
        , _forgeOutPacName   = Just "ForgePac14"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action14 = testLog (runForge forgeOpts14) >> patchLastModified testDir ("forge" </> "ForgePac14" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action14 "forge" [
          "forge" </> "ForgePac14" </> "ForgePac14.janno"
        ]

    -- -- negative selection with different package versions - use versioned to cancel versioned
    let forgeOpts15 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $
            readEntitiesFromString "*Lamnidis_2018-1.0.1*,-*Lamnidis_2018-1.0.1*,*Lamnidis_2018-1.0.0*")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac15"
        , _forgeOutPacName   = Just "ForgePac15"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action15 = testLog (runForge forgeOpts15) >> patchLastModified testDir ("forge" </> "ForgePac15" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action15 "forge" [
          "forge" </> "ForgePac15" </> "ForgePac15.janno"
        ]

    -- negative selection with different package versions - use unversioned to cancel versioned
    let forgeOpts16 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $
            readEntitiesFromString "*Lamnidis_2018-1.0.1*,-*Lamnidis_2018*,*Lamnidis_2018-1.0.0*")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac16"
        , _forgeOutPacName   = Just "ForgePac16"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action16 = testLog (runForge forgeOpts16) >> patchLastModified testDir ("forge" </> "ForgePac16" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action16 "forge" [
          "forge" </> "ForgePac16" </> "ForgePac16.janno"
        ]

    -- negative selection with different package versions - using the SpecificInd interface
    let forgeOpts17 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $
            readEntitiesFromString "*Lamnidis_2018-1.0.1*,-POP2,-<Lamnidis_2018-1.0.1:POP1:XXX017>,-<Lamnidis_2018-1.0.1:POP3:XXX018>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatEigenstrat
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac17"
        , _forgeOutPacName   = Just "ForgePac17"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action17 = testLog (runForge forgeOpts17) >> patchLastModified testDir ("forge" </> "ForgePac17" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action17 "forge" [
          "forge" </> "ForgePac17" </> "ForgePac17.janno"
        ]

    let forgeOpts18 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "POP3,<XXX004>,<XXX006>,<XXX003>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatPlink
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac18"
        , _forgeOutPacName   = Just "ForgePac18"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = True
    }
    let action18 = testLog (runForge forgeOpts18) >> patchLastModified testDir ("forge" </> "ForgePac18" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action18 "forge" [
          "forge" </> "ForgePac18" </> "ForgePac18.janno",
          "forge" </> "ForgePac18" </> "ForgePac18.fam",
          "forge" </> "ForgePac18" </> "ForgePac18.bed"
        ]

    let forgeOpts19 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDir </> "Schiffels_2016"]
        , _forgeEntityInput  = [EntitiesDirect (fromRight [] $ readEntitiesFromString "<XXX004>")]
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatPlink
        , _forgeOutMode      = PreservePymlOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac19"
        , _forgeOutPacName   = Just "ForgePac19"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action19 = testLog (runForge forgeOpts19) >> patchLastModified testDir ("forge" </> "ForgePac19" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action19 "forge" [
          "forge" </> "ForgePac19" </> "POSEIDON.yml",
          "forge" </> "ForgePac19" </> "ForgePac19.ssf",
          "forge" </> "ForgePac19" </> "ForgePac19.bib",
          "forge" </> "ForgePac19" </> "README.md",
          "forge" </> "ForgePac19" </> "CHANGELOG.md",
          "forge" </> "ForgePac19" </> "ForgePac19.bed",
          "forge" </> "ForgePac19" </> "ForgePac19.bim",
          "forge" </> "ForgePac19" </> "ForgePac19.fam",
          "forge" </> "ForgePac19" </> "ForgePac19.janno"
        ]

    -- the .janno file in Schmid_2028_utf8 includes various utf-8 encoding related issues
    -- some old versions of trident failed on "testà" and "MAMS-47224 "
    -- or turned it into "testÃ" and "MAMS-47224Â "
    let forgeOpts20 = ForgeOptions {
          _forgeGenoSources  = [PacBaseDir $ testPacsDirOther </> "Schmid_2028_utf8"]
        , _forgeEntityInput  = []
        , _forgeSnpFile      = Nothing
        , _forgeIntersect    = False
        , _forgeOutFormat    = GenotypeOutFormatPlink
        , _forgeOutMode      = NormalOut
        , _forgeOutZip       = False
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac20"
        , _forgeOutPacName   = Just "ForgePac20"
        , _forgePackageWise  = False
        , _forgeOutputPlinkPopMode = PlinkPopNameAsFamily
        , _forgeOutputOrdered = False
    }
    let action20 = testLog (runForge forgeOpts20) >> patchLastModified testDir ("forge" </> "ForgePac20" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action20 "forge" [
          "forge" </> "ForgePac20" </> "POSEIDON.yml",
          "forge" </> "ForgePac20" </> "ForgePac20.janno"
        ]

    let forgeOpts21 = forgeOpts1 {
          _forgeOutFormat    = GenotypeOutFormatVCF
        , _forgeOutPacPath   = testDir </> "forge" </> "ForgePac21"
        , _forgeOutPacName   = Just "ForgePac21"
    }
    let action21 = testLog (runForge forgeOpts21) >> patchLastModified testDir ("forge" </> "ForgePac21" </> "POSEIDON.yml")
    runAndChecksumFiles checkFilePath testDir action21 "forge" [
          "forge" </> "ForgePac21" </> "POSEIDON.yml"
        , "forge" </> "ForgePac21" </> "ForgePac21.vcf"
        , "forge" </> "ForgePac21" </> "ForgePac21.janno"
        , "forge" </> "ForgePac21" </> "ForgePac21.ssf"
        , "forge" </> "ForgePac21" </> "ForgePac21.bib"
        ]


testPipelineChronicleAndTimetravel :: FilePath -> FilePath -> IO ()
testPipelineChronicleAndTimetravel testDir checkFilePath = do
    -- create relevant test directories
    createDirectoryIfMissing False $ testDir </> "chronicle"
    createDirectoryIfMissing False $ testDir </> "timetravel"
    -- copy packages into the chronicle dir
    copyDirectoryRecursive testPacsDir (testDir </> "chronicle")
    -- initialize this chronicle dir with Git
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " init --quiet"
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " add --all"
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " commit -m \"first commit\" --quiet"
    -- test the creation of a chronicle file
    let chronicleOpts1 = ChronicleOptions {
          _chronicleBaseDirs  = [testDir </> "chronicle"]
        , _chronicleOperation = CreateChron $ testDir </> "chronicle" </> "chronicle1.yml"
    }
    let action1 = testLog (runChronicle chronicleOpts1) >>
            patchLastModified testDir ("chronicle" </> "chronicle1.yml")
    runAndChecksumFiles checkFilePath testDir action1 "chronicle" [
          "chronicle" </> "chronicle1.yml"
        ]
    -- make another, identical chronicle file to test update below
    let chronicleOpts2 = ChronicleOptions {
          _chronicleBaseDirs  = [testDir </> "chronicle"]
        , _chronicleOperation = CreateChron $ testDir </> "chronicle" </> "chronicle2.yml"
    }
    -- here we don't want testLog, but a custom environment with TestMode Production
    usePoseidonLogger NoLog Production PlinkPopNameAsFamily CharInf (runChronicle chronicleOpts2)
    -- add an additional poseidon package from the init directory
    copyDirectoryRecursive (testDir </> "init" </> "Schiffels") (testDir </> "chronicle" </> "Schiffels")
    -- delete a poseidon package
    removeDirectoryRecursive (testDir </> "chronicle" </> "Schmid_2028")
    -- make another git commit with the changed data
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " add --all"
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " commit -m \"second commit\" --quiet"
    -- update the chronicle file
    let chronicleOpts3 = ChronicleOptions {
          _chronicleBaseDirs  = [testDir </> "chronicle"]
        , _chronicleOperation = UpdateChron $ testDir </> "chronicle" </> "chronicle2.yml"
    }
    usePoseidonLogger NoLog Production PlinkPopNameAsFamily CharInf (runChronicle chronicleOpts3)
    -- and a final git commit to tie everything together
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " add --all"
    callCommand $ "git -C " ++ (testDir </> "chronicle") ++ " commit -m \"third commit\" --quiet"
    -- use timetravel to reconstruct the archive described by chronicle2.yml
    let timetravelOpts1 = TimetravelOptions {
           _timetravelBaseDirs      = [testDir </> "timetravel"]
         , _timetravelSourceDir     = testDir </> "chronicle"
         , _timetravelChronicleFile = testDir </> "chronicle" </> "chronicle2.yml"
    }
    let action2 = testLog (runTimetravel timetravelOpts1)
    runAndChecksumFiles checkFilePath testDir action2 "timetravel" [
            -- normal package in version A
            "timetravel" </> "Lamnidis_2018-1.0.0" </> "POSEIDON.yml"
            -- normal package in version B
          , "timetravel" </> "Lamnidis_2018-1.0.1" </> "POSEIDON.yml"
            -- package added in new commit
          , "timetravel" </> "Schiffels-1.1.1" </> "POSEIDON.yml"
            -- package removed in last commit, real timetravel necessary
          , "timetravel" </> "Schmid_2028-1.0.0" </> "POSEIDON.yml"
        ]
    -- delete .git directory in chronicle to clean up in the end
    removeDirectoryRecursive (testDir </> "chronicle" </> ".git")

archives :: Either ArchiveConfig FilePath
archives = Left $ ArchiveConfig [
      ArchiveSpec "testArchive1" [
        "test/testDat/testPackages/ancient/Lamnidis_2018"
      , "test/testDat/testPackages/ancient/Lamnidis_2018_newVersion"
      , "test/testDat/testPackages/ancient/Wang_2020"
      ] Nothing Nothing Nothing
    , ArchiveSpec "testArchive2" [
        "test/testDat/testPackages/ancient/Schiffels_2016"
      , "test/testDat/testPackages/ancient/Schmid_2028"
      ] Nothing Nothing Nothing
    ]

 -- Note: We here use our test server (no SSL and different port). The reason is that
 -- sometimes we would like to implement new features that affect the communication
 -- between server and client, and we need tests succeeding before Pull Requests are merged, so
 -- we adopt the policy to run experimental builds on the test server in order to test features
 -- before running them on the main server.
testPipelineFetch :: FilePath -> FilePath -> IO ()
testPipelineFetch testDir checkFilePath = do
    let serverOpts = ServeOptions archives (Just "/tmp/zip_dir") 3000 True Nothing
    -- we prepare an empty MVar, which is filled as soon as the server is ready
    serverReady <- newEmptyMVar
    -- this will start the server on another thread
    threadID <- forkIO (testLog $ runServer serverOpts serverReady)
    finally (
        do
        -- takeMVar will block the main thread until the server is ready
        _ <- takeMVar serverReady
        -- fetch latest version of package from default archive
        let fetchOpts1 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "by_package"]
            , _entityInput  = [EntitiesDirect [Pac $ PacNameAndVersion "Lamnidis_2018" Nothing]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" Nothing
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts1) "fetch" [
              "fetch" </> "by_package" </> "Lamnidis_2018-1.0.1" </> "POSEIDON.yml"
            ]
        -- fetch only version of package from other archive
        let fetchOpts2 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "by_package"]
            , _entityInput  = [EntitiesDirect [Pac $ PacNameAndVersion "Schmid_2028" Nothing]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" (Just "testArchive2")
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts2) "fetch" [
              "fetch" </> "by_package" </> "Schmid_2028-1.0.0" </> "POSEIDON.yml"
            , "fetch" </> "by_package" </> "Schmid_2028-1.0.0" </> "Schmid_2028.janno"
            , "fetch" </> "by_package" </> "Schmid_2028-1.0.0" </> "geno.txt"
            ]
        -- fetch old version of package from default archive
        let fetchOpts3 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "by_package"]
            , _entityInput  = [EntitiesDirect [Pac $ PacNameAndVersion "Lamnidis_2018" (Just $ makeVersion [1,0,0])]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" Nothing
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts3) "fetch" [
              "fetch" </> "by_package" </> "Lamnidis_2018-1.0.0" </> "POSEIDON.yml"
            ]
        -- fetch package by individual of package from default archive
        let fetchOpts5 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "by_individual"]
            , _entityInput  = [EntitiesDirect [Ind "SAMPLE2"]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" Nothing
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts5) "fetch" [
              "fetch" </> "by_individual" </> "Wang_2020-0.1.0" </> "POSEIDON.yml"
            ]
        -- fetch package by individual through the SpecificInd interface from other archive
        let fetchOpts6 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "by_individual"]
            , _entityInput  = [EntitiesDirect [SpecificInd "XXX001" "POP1" (PacNameAndVersion "Schmid_2028" Nothing)]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" (Just "testArchive2")
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts6) "fetch" [
              "fetch" </> "by_individual" </> "Schmid_2028-1.0.0" </> "POSEIDON.yml"
            ]
        -- fetch package by individual from old package version through the SpecificInd interface from default archive
        let fetchOpts7 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "by_individual"]
            , _entityInput  = [EntitiesDirect [SpecificInd "XXX018" "POP3" (PacNameAndVersion "Lamnidis_2018" (Just $ makeVersion [1,0,0]))]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" Nothing
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts7) "fetch" [
              "fetch" </> "by_individual" </> "Lamnidis_2018-1.0.0" </> "POSEIDON.yml"
            ]
        -- fetch two packages in different versions (order 1)
        let fetchOpts8 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "multi_packages_1"]
            , _entityInput  = [EntitiesDirect [Pac $ PacNameAndVersion "Lamnidis_2018" Nothing, Pac $ PacNameAndVersion "Lamnidis_2018" (Just $ makeVersion [1,0,0])]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" Nothing
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts8) "fetch" [
              "fetch" </> "multi_packages_1" </> "Lamnidis_2018-1.0.0" </> "POSEIDON.yml"
            , "fetch" </> "multi_packages_1" </> "Lamnidis_2018-1.0.1" </> "POSEIDON.yml"
            ]
        -- fetch two packages in different versions (order 2)
        let fetchOpts9 = FetchOptions {
              _jaBaseDirs   = [testDir </> "fetch" </> "multi_packages_2"]
            , _entityInput  = [EntitiesDirect [Pac $ PacNameAndVersion "Lamnidis_2018" (Just $ makeVersion [1,0,0]), Pac $ PacNameAndVersion "Lamnidis_2018" Nothing]]
            , _archiveEnd   = ArchiveEndpoint "http://localhost:3000" Nothing
            }
        runAndChecksumFiles checkFilePath testDir (testLog $ runFetch fetchOpts9) "fetch" [
              "fetch" </> "multi_packages_2" </> "Lamnidis_2018-1.0.0" </> "POSEIDON.yml"
            , "fetch" </> "multi_packages_2" </> "Lamnidis_2018-1.0.1" </> "POSEIDON.yml"
            ]
        ) (
        -- kill server thread
        killThread threadID
        )

testPipelineListRemote :: FilePath -> FilePath -> IO ()
testPipelineListRemote testDir checkFilePath = do
    let serverOpts = ServeOptions archives Nothing 3001 True Nothing
    -- see above
    serverReady <- newEmptyMVar
    threadID <- forkIO (testLog $ runServer serverOpts serverReady)
    _ <- takeMVar serverReady
    finally (
        do
        -- list from default archive
        let listOpts1 = ListOptions {
              _listRepoLocation = RepoRemote (ArchiveEndpoint "http://localhost:3001" Nothing)
            , _listListEntity   = ListPackages
            , _listRawOutput    = False
            , _listOnlyLatest   = False
            }
        runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts1) "listRemote" 1
        let listOpts2 = listOpts1 {
              _listListEntity    = ListGroups
            }
        runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts2) "listRemote" 2
        let listOpts3 = listOpts1 {
              _listListEntity    = ListIndividuals (AddColList ["Publication"])
            , _listRawOutput     = True
            }
        runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts3) "listRemote" 3

        -- list from alternative archive
        let listOpts4 = ListOptions {
              _listRepoLocation = RepoRemote (ArchiveEndpoint "http://localhost:3001" (Just "testArchive2"))
            , _listListEntity   = ListPackages
            , _listRawOutput    = False
            , _listOnlyLatest   = False
            }
        runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts4) "listRemote" 4

        let listOpts5 = listOpts1 {
              _listListEntity    = ListIndividuals AddColAll
            , _listRawOutput     = True
            }
        runAndChecksumStdOut checkFilePath testDir (testLog $ runList listOpts5) "listRemote" 5
        ) (
        killThread threadID
        )

testPipelineJannocoalesce :: FilePath -> FilePath -> IO ()
testPipelineJannocoalesce testDir checkFilePath = do
    -- simple coalesce
    let jannocoalesceOpts1 = JannoCoalesceOptions {
            _jannocoalesceSource           = JannoSourceSingle "test/testDat/testJannoFiles/normal_full.janno",
            _jannocoalesceTarget           = "test/testDat/testJannoFiles/minimal_full.janno",
            _jannocoalesceOutSpec          = Just (testDir </> "jannocoalesce" </> "target1.janno"),
            _jannocoalesceJannoColumns     = AllJannoColumns,
            _jannocoalesceOverwriteColumns = False,
            _jannocoalesceSourceKey        = "Poseidon_ID",
            _jannocoalesceTargetKey        = "Poseidon_ID",
            _jannocoalesceIdStrip          = Nothing
        }
    runAndChecksumFiles checkFilePath testDir (testLog $ runJannocoalesce jannocoalesceOpts1) "jannocoalesce" [
          "jannocoalesce" </> "target1.janno"
        ]
    -- only coalesce certain columns (--includeColumns)
    let jannocoalesceOpts2 = JannoCoalesceOptions {
            _jannocoalesceSource           = JannoSourceSingle "test/testDat/testJannoFiles/normal_full.janno",
            _jannocoalesceTarget           = "test/testDat/testJannoFiles/minimal_full.janno",
            _jannocoalesceOutSpec          = Just (testDir </> "jannocoalesce" </> "target2.janno"),
            _jannocoalesceJannoColumns     = IncludeJannoColumns ["Latitude", "Longitude"],
            _jannocoalesceOverwriteColumns = False,
            _jannocoalesceSourceKey        = "Poseidon_ID",
            _jannocoalesceTargetKey        = "Poseidon_ID",
            _jannocoalesceIdStrip          = Nothing
        }
    runAndChecksumFiles checkFilePath testDir (testLog $ runJannocoalesce jannocoalesceOpts2) "jannocoalesce" [
          "jannocoalesce" </> "target2.janno"
        ]
    -- do not coalesce certain columns (--excludeColumns)
    let jannocoalesceOpts3 = JannoCoalesceOptions {
            _jannocoalesceSource           = JannoSourceSingle "test/testDat/testJannoFiles/normal_full.janno",
            _jannocoalesceTarget           = "test/testDat/testJannoFiles/minimal_full.janno",
            _jannocoalesceOutSpec          = Just (testDir </> "jannocoalesce" </> "target3.janno"),
            _jannocoalesceJannoColumns     = ExcludeJannoColumns ["Latitude", "Longitude"],
            _jannocoalesceOverwriteColumns = False,
            _jannocoalesceSourceKey        = "Poseidon_ID",
            _jannocoalesceTargetKey        = "Poseidon_ID",
            _jannocoalesceIdStrip          = Nothing
        }
    runAndChecksumFiles checkFilePath testDir (testLog $ runJannocoalesce jannocoalesceOpts3) "jannocoalesce" [
          "jannocoalesce" </> "target3.janno"
        ]
