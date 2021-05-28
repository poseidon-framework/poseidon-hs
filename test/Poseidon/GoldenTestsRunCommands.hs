module Poseidon.GoldenTestsRunCommands (createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile) where

import           Poseidon.EntitiesList      (PoseidonEntity (..))
import           Poseidon.CLI.Fetch         (FetchOptions (..), runFetch)
import           Poseidon.CLI.List          (ListOptions (..), runList, 
                                             RepoLocationSpec (..), ListEntity (..))
import           Poseidon.Package           (getChecksum)

import           GHC.IO.Handle              (hDuplicateTo, hDuplicate, hClose)
import           System.Directory           (createDirectory, removeDirectoryRecursive, doesDirectoryExist)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (stdout, IOMode(WriteMode), withFile, openFile, stderr)

tempTestDir :: FilePath
tempTestDir = "/tmp/trident_CLI_test"

staticCheckSumFile :: FilePath
staticCheckSumFile = "test/testDat/staticCheckSumFile.txt"

dynamicCheckSumFile :: FilePath
dynamicCheckSumFile = "/tmp/poseidon_trident_dynamicCheckSumFile.txt"

-- createStaticCheckSumFile :: IO ()
-- createStaticCheckSumFile = runCLICommands staticCheckSumFile 

createDynamicCheckSumFile :: IO ()
createDynamicCheckSumFile = runCLICommands dynamicCheckSumFile 

fetchOpts1 :: FetchOptions
fetchOpts1 = FetchOptions
    { _jaBaseDirs       = [tempTestDir]
    , _entityList       = [Pac "2019_Nikitin_LBK"]
    , _entityFiles      = []
    , _remoteURL        = "https://c107-224.cloud.gwdg.de"
    , _upgrade          = True
    , _downloadAllPacs  = False 
    }

listOpts1 :: ListOptions
listOpts1 = ListOptions
    { _loRepoLocation  = RepoLocal [tempTestDir  </> "2019_Nikitin_LBK"]
    , _loListEntity    = ListPackages
    , _loRawOutput     = False
    , _optIgnoreGeno   = False
    }

listOpts2 :: ListOptions
listOpts2 = ListOptions
    { _loRepoLocation  = RepoLocal [tempTestDir  </> "2019_Nikitin_LBK"]
    , _loListEntity    = ListGroups
    , _loRawOutput     = False
    , _optIgnoreGeno   = False
    }

runCLICommands :: FilePath -> IO ()
runCLICommands checkFilePath = do
    -- create temp dir for test output
    testDirExists <- doesDirectoryExist tempTestDir
    if testDirExists
    then do 
        removeDirectoryRecursive tempTestDir
        createDirectory tempTestDir
    else createDirectory tempTestDir
    -- create/overwrite checksum file
    writeFile checkFilePath "Checksums for trident CLI output \n\ 
        \Automatically generated with: ... \n\
        \"
    -- create error sink
    devNull <- openFile "/dev/null" WriteMode
    hDuplicateTo devNull stderr
    -- fetch
    runAndChecksumFiles checkFilePath tempTestDir (runFetch fetchOpts1) [
          "2019_Nikitin_LBK" </> "POSEIDON.yml"
        , "2019_Nikitin_LBK" </> "Nikitin_LBK.janno"
        , "2019_Nikitin_LBK" </> "Nikitin_LBK.fam"
        ]
    -- list
    runAndChecksumStdOut checkFilePath tempTestDir (runList listOpts1) "tridentList1"
    runAndChecksumStdOut checkFilePath tempTestDir (runList listOpts2) "tridentList2"
    -- close error sink
    hClose devNull

runAndChecksumFiles :: FilePath -> FilePath -> IO () -> [FilePath] -> IO ()
runAndChecksumFiles checkSumFilePath testDir action outFiles = do
    -- run action
    action
    -- append checksums to checksum file
    mapM_ (appendChecksum checkSumFilePath testDir) outFiles
    where
        appendChecksum :: FilePath -> FilePath -> FilePath -> IO ()
        appendChecksum checkSumFilePath_ testDir_ outFile = do
            checksum <- getChecksum $ testDir_ </> outFile
            appendFile checkSumFilePath_ $ "\n" ++ checksum ++ " " ++ outFile

runAndChecksumStdOut :: FilePath -> FilePath -> IO () -> String -> IO ()
runAndChecksumStdOut checkSumFilePath testDir action outFileName = do
    -- store stdout in a specific output file
    withFile (testDir </> outFileName) WriteMode $ \handle -> do
        -- backup stdout handle
        stdout_old <- hDuplicate stdout
        -- redirect stdout to file
        hDuplicateTo handle stdout
        -- run action
        action
        -- load backup again
        hDuplicateTo stdout_old stdout
    -- get checksum of output file
    checksum <- getChecksum $ testDir </> outFileName
    -- write checksum to checksumfile
    appendFile checkSumFilePath $ "\n" ++ checksum ++ " " ++ outFileName 
