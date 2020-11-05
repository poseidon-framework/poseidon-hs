{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.CmdJanno (runJanno, JannoOptions(..)) where

import           Poseidon.Package          (PoseidonSample(..))
import qualified Data.ByteString.Lazy.Char8 as B
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

-- replace :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
-- replace from to s =
--    let (h, t) = B.breakSubstring from s
--    in  B.concat [h, to, B.drop (B.length from) t]

-- replaceNA :: B.ByteString -> B.ByteString
-- replaceNA jannoFile =
--    let na = B.pack "n/a" in
--    replace na B.empty jannoFile

-- | The main function running the janno command
runJanno :: JannoOptions -> IO ()
runJanno (JannoOptions jannoPath) = do 
    jannoFile <- B.readFile jannoPath
    -- replace n/a with empty
    -- let jannoFile = replaceNA jannoFile

    case Csv.decodeWith decodingOptions Csv.HasHeader jannoFile of
        Left err -> do
            Prelude.putStrLn ("Unable to parse data: " ++ err)
        Right (poseidonSamples :: Vector PoseidonSample) -> do
            Prelude.putStrLn ("Parsed janno file: data for " ++ show (V.length poseidonSamples) ++ " poseidonSamples")