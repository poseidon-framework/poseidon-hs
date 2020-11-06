{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.CmdJanno (runJanno, JannoOptions(..)) where

import           Poseidon.Package          (PoseidonSample(..))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as Csv
import           Data.Vector (Vector, toList)
import           Data.Char ( ord )
import qualified Data.List as L

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

summarisePoseidonSamples :: [PoseidonSample] -> IO()
summarisePoseidonSamples v = do
    putStrLn ("Number of samples: " ++ (show $ length v))
    putStrLn ("Individuals: " ++ (L.intercalate ", " $ map posSamIndividualID v))

-- | The main function running the janno command
runJanno :: JannoOptions -> IO ()
runJanno (JannoOptions jannoPath) = do 
    jannoFile <- B.readFile jannoPath
    -- replace n/a with empty
    let jannoFileUpdated = replaceNA jannoFile

    case Csv.decodeWith decodingOptions Csv.HasHeader jannoFileUpdated of
        Left err -> do
            putStrLn ("Unable to parse data: " ++ err)
        Right (poseidonSamples :: Vector PoseidonSample) -> do
            summarisePoseidonSamples $ toList poseidonSamples
