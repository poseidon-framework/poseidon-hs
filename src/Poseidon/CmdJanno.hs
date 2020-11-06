{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.CmdJanno (runJanno, JannoOptions(..)) where

import           Poseidon.Package          (PoseidonSample(..))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as Csv
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Char ( ord )
import qualified Data.Text as T

-- | A datatype representing command line options for the janno command
data JannoOptions = JannoOptions
    { _jannoPath   :: FilePath 
    }

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions { 
    Csv.decDelimiter = fromIntegral (ord '\t')
}

instance Csv.FromRecord PoseidonSample

bytestringToDouble :: [B.ByteString] -> [Double]
bytestringToDouble [] = []
bytestringToDouble (x:xs) = (read (B.unpack x) :: Double) : bytestringToDouble xs

instance Csv.FromField [Double] where
    parseField = fmap bytestringToDouble . fmap (\x -> B.splitWith (==';') x) . Csv.parseField

bytestringToInteger :: [B.ByteString] -> [Integer]
bytestringToInteger [] = []
bytestringToInteger (x:xs) = (read (B.unpack x) :: Integer) : bytestringToInteger xs

instance Csv.FromField [Integer] where
    parseField = fmap bytestringToInteger . fmap (\x -> B.splitWith (==';') x) . Csv.parseField

bytestringToString :: [B.ByteString] -> [String]
bytestringToString [] = []
bytestringToString (x:xs) = (B.unpack x) : bytestringToString xs

instance Csv.FromField [String] where
    parseField = fmap bytestringToString . fmap (\x -> B.splitWith (==';') x) . Csv.parseField

replaceNA :: B.ByteString -> B.ByteString
replaceNA tsv =
   let tsvRows = B.lines tsv
       tsvCells = map (\x -> B.splitWith (=='\t') x) tsvRows
       tsvCellsUpdated = map (\x -> map (\y -> if y == (B.pack "n/a") then B.empty else y) x) tsvCells
       tsvRowsUpdated = map (\x -> B.intercalate (B.pack "\t") x) tsvCellsUpdated
   in B.unlines tsvRowsUpdated

-- | The main function running the janno command
runJanno :: JannoOptions -> IO ()
runJanno (JannoOptions jannoPath) = do 
    jannoFile <- B.readFile jannoPath
    -- replace n/a with empty
    let jannoFileUpdated = replaceNA jannoFile

    case Csv.decodeWith decodingOptions Csv.HasHeader jannoFileUpdated of
        Left err -> do
            Prelude.putStrLn ("Unable to parse data: " ++ err)
        Right (poseidonSamples :: Vector PoseidonSample) -> do
            -- Prelude.putStrLn ("Parsed janno file: data for " ++ show (V.length poseidonSamples) ++ " poseidonSamples")
            Prelude.putStrLn (show poseidonSamples)