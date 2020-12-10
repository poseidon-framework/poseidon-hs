module Poseidon.CLI.Init (
    runInit, InitOptions (..),
    ) where

data InitOptions = InitOptions
    { _inGenoFile :: FilePath
    , _inSnpFile :: FilePath
    , _inIndFile :: FilePath
    , _outPacPath :: FilePath
    , _outPacName :: String
    }

runInit :: InitOptions -> IO ()
runInit (InitOptions genoFile snpFile indFile outPath outName) = do
    putStrLn "test"