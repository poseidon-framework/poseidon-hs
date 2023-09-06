{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Survey where

import           Poseidon.BibFile          (BibTeX)
import           Poseidon.GenotypeData     (GenotypeDataSpec (..))
import           Poseidon.Janno            (CsvNamedRecord, JannoRows (..),
                                            JannoSex, JannoStringList)
import           Poseidon.Package          (PackageReadOptions (..),
                                            PoseidonPackage (..),
                                            defaultPackageReadOptions,
                                            readPoseidonPackageCollection)
import           Poseidon.Utils            (PoseidonIO, logInfo)

import           Control.Monad             (forM)
import           Control.Monad.IO.Class    (liftIO)
import           Data.List                 (intercalate, unfoldr, zip5)
import           Data.Ratio                (Ratio, (%))
import           Generics.SOP              (All, Generic (Code, from),
                                            HCollapse (hcollapse),
                                            HPure (hpure), I, K (K), NP,
                                            Proxy (..), SListI, hcmap, hzipWith,
                                            unI, unSOP, unZ)
import           Poseidon.SequencingSource (SeqSourceRows (..))
import           System.Directory          (doesFileExist)
import           System.FilePath           ((</>))
import           Text.Layout.Table         (asciiRoundS, column, def,
                                            expandUntil, rowsG, tableString,
                                            titlesH)

-- | A datatype representing command line options for the survey command
data SurveyOptions = SurveyOptions
    { _surveyBaseDirs  :: [FilePath]
    , _surveyRawOutput :: Bool
    }

pacReadOpts :: PackageReadOptions
pacReadOpts = defaultPackageReadOptions {
      _readOptStopOnDuplicates = False
    , _readOptIgnoreChecksums  = True
    , _readOptIgnoreGeno       = True
    , _readOptGenoCheck        = False
    }

-- | The main function running the janno command
runSurvey :: SurveyOptions -> PoseidonIO ()
runSurvey (SurveyOptions baseDirs rawOutput) = do

    allPackages <- readPoseidonPackageCollection pacReadOpts baseDirs
    -- collect information
    let packageNames = map posPacNameAndVersion allPackages
    -- geno
    let genotypeDataTuples = [(posPacBaseDir pac, posPacGenotypeData pac) | pac <- allPackages]
    genoFilesExist <- liftIO $ sequence [doesFileExist (d </> genoFile gd) | (d, gd) <- genotypeDataTuples]
    snpFilesExist  <- liftIO $ sequence [doesFileExist (d </> snpFile gd) | (d, gd) <- genotypeDataTuples]
    indFilesExist  <- liftIO $ sequence [doesFileExist (d </> indFile gd) | (d, gd) <- genotypeDataTuples]
    let genoTypeDataExists = map (\(a,b,c) -> a && b && c) $ zip3 genoFilesExist snpFilesExist indFilesExist
    -- janno
    let jannos = map posPacJanno allPackages
    -- ssf
    let ssfs = map posPacSeqSource allPackages
    -- bib
    let bibs = map posPacBib allPackages
    -- print information
    (tableH, tableB) <- do
        let tableH = ["Package", "Survey"]
        tableB <- forM (zip5 packageNames genoTypeDataExists jannos ssfs bibs) $ \(p, g, j, s, b) -> do
            return [show p, renderPackageWithCompleteness g j s b]
        return (tableH, tableB)
    let colSpecs = replicate 2 (column (expandUntil 60) def def def)
    if rawOutput
    then liftIO $ putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
    else liftIO $ putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
    -- print help
    logInfo "see trident survey -h for a list of column names"

renderPackageWithCompleteness :: Bool -> JannoRows -> SeqSourceRows -> BibTeX -> String
renderPackageWithCompleteness genoTypeDataExists janno (SeqSourceRows seqSource) bib =
       (if genoTypeDataExists then "G" else ".")
    ++ (if not (null seqSource) then "S" else ".")
    ++ (if not (null bib) then "B" else ".")
    ++ "|"
    ++ insertEveryN 5 '|' (renderJannoCompleteness janno)
    where
        -- https://stackoverflow.com/questions/12659562/insert-specific-element-y-after-every-n-elements-in-a-list
        insertEveryN :: Int -> a -> [a] -> [a]
        insertEveryN n y xs = intercalate [y] . groups n $ xs
            where groups n_ xs_ = takeWhile (not . null) . unfoldr (Just . splitAt n_) $ xs_

renderJannoCompleteness :: JannoRows -> String
renderJannoCompleteness (JannoRows rows) =
    let ratioString = map prop2Char $ getRatiosForEachField rows
    in init ratioString -- remove last entry covering the additional columns (CsvNamedRecord)
    where
        -- the following magic was heavily inspired by https://stackoverflow.com/a/41524511/3216883
        getRatiosForEachField :: (Generics.SOP.Generic a, Code a ~ '[ xs ], All PresenceCountable xs) => [a] -> [Ratio Int]
        getRatiosForEachField =
            hcollapse
          . hcmap (Proxy :: Proxy PresenceCountable) (K . measureFillState)
          . hunzip
          . map (unZ . unSOP . from)
        hunzip :: SListI xs => [NP I xs] -> NP [] xs
        hunzip = foldr (hzipWith ((:) . unI)) (hpure [])
        measureFillState :: PresenceCountable a => [a] -> Ratio Int
        measureFillState vals =
            let nrValues = length vals
                nrFilledValues = sum $ map countPresence vals
            in nrFilledValues % nrValues
        prop2Char :: Ratio Int -> Char
        prop2Char r
            | r == 0    = '.'
            | r < 0.25  = '░'
            | r < 0.5   = '▒'
            | r < 1     = '▓'
            | r == 1    = '█'
            | otherwise = '?'

-- A typeclass to determine if a field in a .janno row is filled
class PresenceCountable a where
    countPresence :: a -> Int
instance PresenceCountable (Maybe a) where
    countPresence Nothing  = 0
    countPresence (Just _) = 1
instance PresenceCountable String where
    countPresence _ = 1
instance PresenceCountable JannoSex where
    countPresence _ = 1
instance PresenceCountable JannoStringList where
    countPresence _ = 1
instance PresenceCountable CsvNamedRecord where
    countPresence _ = 0
