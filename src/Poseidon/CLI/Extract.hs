module Poseidon.CLI.Extract (runExtract, ExtractOptions(..)) where

-- | A datatype representing command line options for the survey command
data ExtractOptions = ExtractOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runExtract :: ExtractOptions -> IO ()
runExtract (ExtractOptions baseDirs) = do
    putStrLn "extract"
