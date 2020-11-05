module Poseidon.CmdJanno (runJanno, JannoOptions(..)) where

-- | A datatype representing command line options for the janno command
data JannoOptions = JannoOptions
    { _jannoPath   :: [FilePath] 
    }

-- | The main function running the janno command
runJanno :: JannoOptions -> IO ()
runJanno (JannoOptions jannoPath) = do 
    putStrLn "Hello"