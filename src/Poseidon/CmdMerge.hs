module Poseidon.CmdMerge (runMerge, MergeOptions(..)) where

-- | A datatype representing command line options for the survey command
data MergeOptions = MergeOptions
    { _jaBaseDirs  :: [FilePath]
    }

-- | The main function running the janno command
runMerge :: MergeOptions -> IO ()
runMerge (MergeOptions baseDirs) = do
    putStrLn "merge"
