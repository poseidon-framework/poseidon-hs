{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.CmdJanno (runJanno, JannoOptions(..)) where

import           Poseidon.Package (loadPoseidonPackages, PoseidonPackage(..), PoseidonSample(..))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Csv as Csv
import           Data.Vector (Vector, toList)
import           Data.Char ( ord )
import qualified Data.List as L
import qualified Data.Maybe as DM
import           System.IO (hPutStrLn, stderr)

-- | A datatype representing command line options for the janno command
data JannoOptions = JannoOptions
    { _jaBaseDirs  :: [FilePath]
    -- , _jannoPath   :: FilePath 
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

pasteFirst3 :: [String] -> String
pasteFirst3 [] = "no values"
pasteFirst3 xs = 
    (L.intercalate ", " $ take 3 xs) ++ if (length xs > 3) then ", ..." else ""

removeNothing :: [Maybe a] -> [a]
removeNothing xs =
    let onlyJust = filter DM.isJust xs
    in DM.catMaybes onlyJust

summarisePoseidonSamples :: [PoseidonSample] -> IO ()
summarisePoseidonSamples xs = do
    putStrLn ("Number of samples: " ++ (show $ length xs))
    putStrLn $ "Individuals: " ++ pasteFirst3 (map posSamIndividualID xs)
    putStrLn $ "Countries: " ++ pasteFirst3 (removeNothing (map posSamCountry xs))

-- | The main function running the janno command
runJanno :: JannoOptions -> IO ()
runJanno (JannoOptions baseDirs) = do 
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    let jannoFilePaths = map posPacJannoFile packages
    let jannoFiles = readMultipleJannoFiles jannoFilePaths
    jannoSamples <- fmap concat jannoFiles
    summarisePoseidonSamples jannoSamples

readMultipleJannoFiles :: [FilePath] -> IO [[PoseidonSample]]
readMultipleJannoFiles jannoPaths = do
    sequence (map readOneJannoFile jannoPaths)

readOneJannoFile :: FilePath -> IO [PoseidonSample]
readOneJannoFile jannoPath = do
    jannoFile <- B.readFile jannoPath
    -- replace n/a with empty
    let jannoFileUpdated = replaceNA jannoFile
    case Csv.decodeWith decodingOptions Csv.HasHeader jannoFileUpdated of
        -- Left err -> do
        --     putStrLn ("Unable to parse data: " ++ err)
        Right (poseidonSamples :: Vector PoseidonSample) -> do
            return $ toList poseidonSamples
