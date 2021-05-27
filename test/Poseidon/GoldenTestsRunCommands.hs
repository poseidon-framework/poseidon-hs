module Poseidon.GoldenTestsRunCommands (createDynamicCheckSumFile, staticCheckSumFile, dynamicCheckSumFile) where

import           Poseidon.EntitiesList      (PoseidonEntity (..))--(PoseidonEntity (..), EntitiesList, 
                                             --readEntitiesFromFile)
import           Poseidon.CLI.Fetch         (FetchOptions (..), runFetch)
-- import           Poseidon.CLI.List          (ListOptions (..), runList, 
--                                              RepoLocationSpec (..), ListEntity (..))
import           Poseidon.Package           (getChecksum)

--import           System.IO                (IOMode(WriteMode), withFile, hPutStrLn, stderr, writeFile)
import           System.Directory           (createDirectory,removeDirectoryRecursive)
import           System.FilePath.Posix      ((</>))

temporaryTestDir :: FilePath
temporaryTestDir = "/tmp/trident_CLI_test"

-- testBaseDir :: [FilePath]
-- testBaseDir = ["test/testDat/testModules/ancient"]

staticCheckSumFile :: FilePath 
staticCheckSumFile = "test/testDat/staticCheckSumFile.txt"

dynamicCheckSumFile :: FilePath 
dynamicCheckSumFile = "/tmp/poseidon_trident_dynamicCheckSumFile.txt"

-- createStaticCheckSumFile :: IO ()
-- createStaticCheckSumFile = runCLICommands staticCheckSumFile 

createDynamicCheckSumFile :: IO ()
createDynamicCheckSumFile = runCLICommands dynamicCheckSumFile 

-- listOpts1 = ListOptions
--     { _loRepoLocation  = RepoLocal testBaseDir
--     , _loListEntity    = ListPackages
--     , _loRawOutput     = False
--     , _optIgnoreGeno   = False
--     }

fetchOpts1 :: FetchOptions
fetchOpts1 = FetchOptions
    { _jaBaseDirs       = [temporaryTestDir]
    , _entityList       = [Pac "2019_Nikitin_LBK"]
    , _entityFiles      = []
    , _remoteURL        = "https://c107-224.cloud.gwdg.de"
    , _upgrade          = True
    , _downloadAllPacs  = False 
    }

runCLICommands :: FilePath -> IO ()
runCLICommands checkSumFilePath = do
    createDirectory temporaryTestDir
    --withFile (temporaryTestDir </> "trident_list") WriteMode (runList listOpts1)
    runFetch fetchOpts1
    fetchCheck1 <- getChecksum $ temporaryTestDir </> "2019_Nikitin_LBK" </> "POSEIDON.yml"
    writeFile checkSumFilePath $ fetchCheck1 ++ " fetchCheck1 POSEIDON.yml"
    removeDirectoryRecursive temporaryTestDir
