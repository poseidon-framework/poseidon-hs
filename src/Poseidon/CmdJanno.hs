{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.CmdJanno (runJanno, JannoOptions(..)) where

import           Poseidon.Package          (PoseidonSample(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as Csv
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Char ( ord )

-- | A datatype representing command line options for the janno command
data JannoOptions = JannoOptions
    { _jannoPath   :: FilePath 
    }

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions { 
    Csv.decDelimiter = fromIntegral (ord '\t')
}

instance Csv.FromRecord PoseidonSample

-- | The main function running the janno command
runJanno :: JannoOptions -> IO ()
runJanno (JannoOptions jannoPath) = do 
    jannoFile <- BL.readFile jannoPath

    case Csv.decodeWith decodingOptions Csv.HasHeader jannoFile of
        Left err -> do
            putStrLn ("Unable to parse data: " ++ err)
        Right (poseidonSamples :: Vector PoseidonSample) -> do
            putStrLn ("Parsed janno file: data for " ++ show (V.length poseidonSamples) ++ " poseidonSamples")