module Poseidon.GoldenTestsRunCommands (createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile) where

import           Poseidon.EntitiesList      (PoseidonEntity (..), EntitiesList, 
                                             readEntitiesFromFile)
import           Poseidon.CLI.Fetch         (FetchOptions (..), runFetch)
import           Poseidon.CLI.List          (ListOptions (..), runList, 
                                             RepoLocationSpec (..), ListEntity (..))
import           Poseidon.Package           (getChecksum)

import           GHC.IO.Handle              (hDuplicateTo, hDuplicate, hClose)
import           System.Directory           (createDirectory,removeDirectoryRecursive)
import           System.FilePath.Posix      ((</>))
import           System.IO                  (stdout, IOMode(WriteMode), withFile, openFile, stderr)

temporaryTestDir :: FilePath
temporaryTestDir = "/tmp/trident_CLI_test"

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
    { _jaBaseDirs       = [temporaryTestDir]
    , _entityList       = [Pac "2019_Nikitin_LBK"]
    , _entityFiles      = []
    , _remoteURL        = "https://c107-224.cloud.gwdg.de"
    , _upgrade          = True
    , _downloadAllPacs  = False 
    }

listOpts1 :: ListOptions
listOpts1 = ListOptions
    { _loRepoLocation  = RepoLocal [temporaryTestDir  </> "2019_Nikitin_LBK"]
    , _loListEntity    = ListPackages
    , _loRawOutput     = False
    , _optIgnoreGeno   = False
    }

runCLICommands :: FilePath -> IO ()
runCLICommands checkSumFilePath = do
    -- create temp dir for test output
    createDirectory temporaryTestDir
    -- create error sink
    devNull <- openFile "/dev/null" WriteMode
    hDuplicateTo devNull stderr
    -- fetch
    runFetch fetchOpts1
    fetchCheck1 <- getChecksum $ temporaryTestDir </> "2019_Nikitin_LBK" </> "POSEIDON.yml"
    writeFile checkSumFilePath $ fetchCheck1 ++ " fetchCheck1 POSEIDON.yml"
    -- list
    withFile (temporaryTestDir </> "listStdOut.txt") WriteMode $ \handle -> do
        hDuplicateTo handle stdout -- redirect stdout to file
        runList listOpts1
    listCheck1 <- getChecksum $ temporaryTestDir </> "listStdOut.txt"
    appendFile checkSumFilePath $ "\n" ++ listCheck1 ++ " listCheck1 listStdOut.txt"
    -- close error sink
    hClose devNull
    -- delete temp dir for test output
    removeDirectoryRecursive temporaryTestDir
