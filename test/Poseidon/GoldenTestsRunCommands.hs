module Poseidon.GoldenTestsRunCommands (createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile) where

import           Poseidon.EntitiesList      (PoseidonEntity (..), EntitiesList, 
                                             readEntitiesFromFile)
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

createStaticCheckSumFile :: IO ()
createStaticCheckSumFile = runCLICommands staticCheckSumFile 

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
    -- create error sink
    devNull <- openFile "/dev/null" WriteMode
    hDuplicateTo devNull stderr
    -- fetch
    runFetch fetchOpts1
    fetchCheck1 <- getChecksum $ tempTestDir </> "2019_Nikitin_LBK" </> "POSEIDON.yml"
    writeFile checkFilePath $ fetchCheck1 ++ " fetchCheck1 POSEIDON.yml"
    -- list
    runAndStoreChecksum checkFilePath tempTestDir "tridentList1" (runList listOpts1)
    runAndStoreChecksum checkFilePath tempTestDir "tridentList2" (runList listOpts2)
    -- close error sink
    hClose devNull

runAndStoreChecksum :: FilePath -> FilePath -> String -> IO () -> IO ()
runAndStoreChecksum checkSumFilePath testDir outFileName action = do
    withFile (testDir </> outFileName) WriteMode $ \handle -> do
        stdout_old <- hDuplicate stdout -- backup stdout
        hDuplicateTo handle stdout -- redirect stdout to file
        action -- run action
        hDuplicateTo stdout_old stdout -- load backup again
    listCheck1 <- getChecksum $ testDir </> outFileName
    appendFile checkSumFilePath $ "\n" ++ listCheck1 ++ " " ++ outFileName
