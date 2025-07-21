{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.SequencingSource where

import           Poseidon.ColumnTypesSSF
import           Poseidon.ColumnTypesUtils  (ListColumn (..))
import           Poseidon.Janno             (CsvNamedRecord (..),
                                             decodingOptions, encodingOptions,
                                             explicitNA, filterLookup,
                                             filterLookupOptional, getCsvNR,
                                             parseCsvParseError,
                                             removeUselessSuffix,
                                             renderCsvParseError)
import           Poseidon.Utils             (PoseidonException (..), PoseidonIO,
                                             logDebug, logError, logWarning,
                                             renderPoseidonException)

import           Control.Exception          (throwIO)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Bifunctor             (second)
import qualified Data.ByteString.Char8      as Bchs
import qualified Data.ByteString.Lazy.Char8 as Bch
import qualified Data.Csv                   as Csv
import           Data.Either                (lefts, rights)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (foldl', nub, sort)
import           Data.Maybe                 (isJust, mapMaybe)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import qualified Text.Parsec                as P

-- | A data type to represent a seqSourceFile
newtype SeqSourceRows = SeqSourceRows {getSeqSourceRowList :: [SeqSourceRow]}
    deriving (Show, Eq, Generic)

instance Semigroup SeqSourceRows where
    (SeqSourceRows j1) <> (SeqSourceRows j2) = SeqSourceRows $ j1 `combineTwoSeqSources` j2
        where
        combineTwoSeqSources :: [SeqSourceRow] -> [SeqSourceRow] -> [SeqSourceRow]
        combineTwoSeqSources seqSource1 seqSource2 =
            let simpleSeqSourceSum = seqSource1 ++ seqSource2
                toAddColNames = HM.keys (HM.unions (map (getCsvNR . sAdditionalColumns) simpleSeqSourceSum))
                toAddEmptyCols = HM.fromList (map (\k -> (k, "n/a")) toAddColNames)
            in map (addEmptyAddColsToSeqSourceRow toAddEmptyCols) simpleSeqSourceSum
        addEmptyAddColsToSeqSourceRow :: Csv.NamedRecord -> SeqSourceRow -> SeqSourceRow
        addEmptyAddColsToSeqSourceRow toAdd x =
            x { sAdditionalColumns = CsvNamedRecord $ fillAddCols toAdd (getCsvNR $ sAdditionalColumns x) }
        fillAddCols :: Csv.NamedRecord -> Csv.NamedRecord -> Csv.NamedRecord
        fillAddCols toAdd cur = HM.union cur (toAdd `HM.difference` cur)

instance Monoid SeqSourceRows where
    mempty = SeqSourceRows []
    mconcat = foldl' mappend mempty

-- | A data type to represent a row in the seqSourceFile
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/seqSourceFile_columns.tsv
-- for more details
data SeqSourceRow = SeqSourceRow
    { sPoseidonID               :: Maybe (ListColumn String)
    , sUDG                      :: Maybe SSFUDG
    , sLibraryBuilt             :: Maybe SSFLibraryBuilt
    , sSampleAccession          :: Maybe AccessionIDSample
    , sStudyAccession           :: Maybe AccessionIDStudy
    , sRunAccession             :: Maybe AccessionIDRun
    , sSampleAlias              :: Maybe SSFSampleAlias
    , sSecondarySampleAccession :: Maybe SSFSecondarySampleAccession
    , sFirstPublic              :: Maybe SimpleDate
    , sLastUpdated              :: Maybe SimpleDate
    , sInstrumentModel          :: Maybe SSFInstrumentModel
    , sLibraryLayout            :: Maybe SSFLibraryLayout
    , sLibrarySource            :: Maybe SSFLibrarySource
    , sInstrumentPlatform       :: Maybe SSFInstrumentPlatform
    , sLibraryName              :: Maybe SSFLibraryName
    , sLibraryStrategy          :: Maybe SSFLibraryStrategy
    , sFastqFTP                 :: Maybe (ListColumn JURI)
    , sFastqASPERA              :: Maybe (ListColumn JURI)
    , sFastqBytes               :: Maybe (ListColumn Integer) -- integer, not int, because it can be a very large number
    , sFastqMD5                 :: Maybe (ListColumn MD5)
    , sReadCount                :: Maybe Integer             -- integer, not int, because it can be a very large number
    , sSubmittedFTP             :: Maybe (ListColumn JURI)
    , sAdditionalColumns        :: CsvNamedRecord
    }
    deriving (Show, Eq, Generic)

-- This header also defines the output column order when writing to csv!
seqSourceHeader :: [Bchs.ByteString]
seqSourceHeader = [
      "poseidon_IDs"
    , "udg"
    , "library_built"
    , "sample_accession"
    , "study_accession"
    , "run_accession"
    , "sample_alias"
    , "secondary_sample_accession"
    , "first_public"
    , "last_updated"
    , "instrument_model"
    , "library_layout"
    , "library_source"
    , "instrument_platform"
    , "library_name"
    , "library_strategy"
    , "fastq_ftp"
    , "fastq_aspera"
    , "fastq_bytes"
    , "fastq_md5"
    , "read_count"
    , "submitted_ftp"
    ]

instance Csv.DefaultOrdered SeqSourceRow where
    headerOrder _ = Csv.header seqSourceHeader

seqSourceHeaderString :: [String]
seqSourceHeaderString = map Bchs.unpack seqSourceHeader

-- This hashmap represents an empty seqSourceFile with all normal, specified columns
seqSourceRefHashMap :: HM.HashMap Bchs.ByteString ()
seqSourceRefHashMap = HM.fromList $ map (\x -> (x, ())) seqSourceHeader

instance Csv.FromNamedRecord SeqSourceRow where
    parseNamedRecord m = SeqSourceRow
        <$> filterLookupOptional m "poseidon_IDs"
        <*> filterLookupOptional m "udg"
        <*> filterLookupOptional m "library_built"
        <*> filterLookupOptional m "sample_accession"
        <*> filterLookupOptional m "study_accession"
        <*> filterLookup         m "run_accession"
        <*> filterLookupOptional m "sample_alias"
        <*> filterLookupOptional m "secondary_sample_accession"
        <*> filterLookupOptional m "first_public"
        <*> filterLookupOptional m "last_updated"
        <*> filterLookupOptional m "instrument_model"
        <*> filterLookupOptional m "library_layout"
        <*> filterLookupOptional m "library_source"
        <*> filterLookupOptional m "instrument_platform"
        <*> filterLookupOptional m "library_name"
        <*> filterLookupOptional m "library_strategy"
        <*> filterLookupOptional m "fastq_ftp"
        <*> filterLookupOptional m "fastq_aspera"
        <*> filterLookupOptional m "fastq_bytes"
        <*> filterLookupOptional m "fastq_md5"
        <*> filterLookupOptional m "read_count"
        <*> filterLookupOptional m "submitted_ftp"
        -- beyond that read everything that is not in the set of defined variables
        -- as a separate hashmap
        <*> pure (CsvNamedRecord (m `HM.difference` seqSourceRefHashMap))

instance Csv.ToNamedRecord SeqSourceRow where
    toNamedRecord s = explicitNA $ Csv.namedRecord [
          "poseidon_IDs"               Csv..= sPoseidonID s
        , "udg"                        Csv..= sUDG s
        , "library_built"              Csv..= sLibraryBuilt s
        , "sample_accession"           Csv..= sSampleAccession s
        , "study_accession"            Csv..= sStudyAccession s
        , "run_accession"              Csv..= sRunAccession s
        , "sample_alias"               Csv..= sSampleAlias s
        , "secondary_sample_accession" Csv..= sSecondarySampleAccession s
        , "first_public"               Csv..= sFirstPublic s
        , "last_updated"               Csv..= sLastUpdated s
        , "instrument_model"           Csv..= sInstrumentModel s
        , "library_layout"             Csv..= sLibraryLayout s
        , "library_source"             Csv..= sLibrarySource s
        , "instrument_platform"        Csv..= sInstrumentPlatform s
        , "library_name"               Csv..= sLibraryName s
        , "library_strategy"           Csv..= sLibraryStrategy s
        , "fastq_ftp"                  Csv..= sFastqFTP s
        , "fastq_aspera"               Csv..= sFastqASPERA s
        , "fastq_bytes"                Csv..= sFastqBytes s
        , "fastq_md5"                  Csv..= sFastqMD5 s
        , "read_count"                 Csv..= sReadCount s
        , "submitted_ftp"              Csv..= sSubmittedFTP s
        -- beyond that add what is in the hashmap of additional columns
        ] `HM.union` (getCsvNR $ sAdditionalColumns s)

-- | A function to write one seqSourceFile
writeSeqSourceFile :: FilePath -> SeqSourceRows -> IO ()
writeSeqSourceFile path (SeqSourceRows rows) = do
    let seqSourceAsBytestring = Csv.encodeByNameWith encodingOptions makeHeaderWithAdditionalColumns rows
    Bch.writeFile path seqSourceAsBytestring
    where
        makeHeaderWithAdditionalColumns :: Csv.Header
        makeHeaderWithAdditionalColumns =
            V.fromList $ seqSourceHeader ++ sort (HM.keys (HM.unions (map (getCsvNR . sAdditionalColumns) rows)))

-- | A function to read one seqSourceFile
readSeqSourceFile :: FilePath -> PoseidonIO SeqSourceRows
readSeqSourceFile seqSourcePath = do
    logDebug $ "Reading: " ++ seqSourcePath
    seqSourceFile <- liftIO $ Bch.readFile seqSourcePath
    let seqSourceFileRows = Bch.lines seqSourceFile
    when (length seqSourceFileRows < 2) $ liftIO $ throwIO $ PoseidonFileConsistencyException seqSourcePath "File has less than two lines"
    logDebug $ show (length seqSourceFileRows - 1) ++ " sequencing entities in this file"
    -- tupel with row number and row bytestring
    let seqSourceFileRowsWithNumber = zip [1..(length seqSourceFileRows)] seqSourceFileRows
    -- filter out empty lines
        seqSourceFileRowsWithNumberFiltered = filter (\(_, y) -> y /= Bch.empty) seqSourceFileRowsWithNumber
    -- create header + individual line combination
        headerOnlyPotentiallyWithQuotes = snd $ head seqSourceFileRowsWithNumberFiltered
        -- removing the quotes like this might cause issues in edge cases
        headerOnly = Bch.filter (/= '"') headerOnlyPotentiallyWithQuotes
        rowsOnly = tail seqSourceFileRowsWithNumberFiltered
        seqSourceFileRowsWithHeader = map (second (\x -> headerOnly <> "\n" <> x)) rowsOnly
    -- read seqSourceFile by rows
    seqSourceRepresentation <- mapM (readSeqSourceFileRow seqSourcePath) seqSourceFileRowsWithHeader
    -- error case management
    if not (null (lefts seqSourceRepresentation))
    then do
        mapM_ (logError . renderPoseidonException) $ take 5 $ lefts seqSourceRepresentation
        liftIO $ throwIO $ PoseidonFileConsistencyException seqSourcePath "Broken lines."
    else do
        let seqSource = SeqSourceRows $ rights seqSourceRepresentation
        warnSeqSourceConsistency seqSourcePath seqSource
        return seqSource

-- | A function to read one row of a seqSourceFile
readSeqSourceFileRow :: FilePath -> (Int, Bch.ByteString) -> PoseidonIO (Either PoseidonException SeqSourceRow)
readSeqSourceFileRow seqSourcePath (lineNumber, row) = do
    let decoded = Csv.decodeByNameWith decodingOptions row
        simplifiedDecoded = (\(_,rs) -> V.head rs) <$> decoded
    case simplifiedDecoded of
        Left e -> do
            let betterError = case P.parse parseCsvParseError "" e of
                    Left _       -> removeUselessSuffix e
                    Right result -> renderCsvParseError result
            return $ Left $ PoseidonFileRowException seqSourcePath (show lineNumber) betterError
        Right seqSourceRow -> do
            return $ Right seqSourceRow

-- Global SSF consistency checks

warnSeqSourceConsistency :: FilePath -> SeqSourceRows -> PoseidonIO ()
warnSeqSourceConsistency seqSourcePath xs = do
    unless (checkRunsUnique xs) $
        logWarning $ "Potential consistency issues in file " ++ seqSourcePath ++ ": " ++
                     "The values in the run_accession column are not unique"
    unless (checkAtLeastOnePoseidonID xs) $
        logWarning $ "Potential consistency issues in file " ++ seqSourcePath ++ ": " ++
                     "The poseidon_IDs column is completely empty. Package and .ssf file are not linked"

checkRunsUnique :: SeqSourceRows -> Bool
checkRunsUnique (SeqSourceRows rows) =
    let justRunAccessions = mapMaybe sRunAccession rows
    in justRunAccessions == nub justRunAccessions

checkAtLeastOnePoseidonID :: SeqSourceRows -> Bool
checkAtLeastOnePoseidonID (SeqSourceRows rows) =
    any (isJust . sPoseidonID) rows
