{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.SequencingSource where

import           Poseidon.Janno             (AccessionID (..),
                                             CsvNamedRecord (..), JURI,
                                             JannoList (..), JannoStringList,
                                             decodingOptions, encodingOptions,
                                             explicitNA, filterLookup,
                                             filterLookupOptional, getCsvNR,
                                             makeAccessionID,
                                             parseCsvParseError,
                                             removeUselessSuffix,
                                             renderCsvParseError)
import           Poseidon.Utils             (PoseidonException (..), PoseidonIO,
                                             logDebug, logError, logWarning,
                                             renderPoseidonException)

import           Control.Exception          (throwIO)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, Options (..), ToJSON,
                                             defaultOptions, genericToEncoding,
                                             toEncoding, withText)
import           Data.Aeson.Encoding        (text)
import           Data.Bifunctor             (second)
import qualified Data.ByteString.Char8      as Bchs
import qualified Data.ByteString.Lazy.Char8 as Bch
import           Data.Char                  (isHexDigit)
import qualified Data.Csv                   as Csv
import           Data.Either                (lefts, rights)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (foldl', nub, sort)
import           Data.Maybe                 (isJust, mapMaybe)
import qualified Data.Text                  as T
import           Data.Time                  (Day)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import qualified Data.Vector                as V
import           Data.Yaml.Aeson            (FromJSON (..))
import           GHC.Generics               (Generic)
import qualified Text.Parsec                as P

-- |A datatype to represent UDG in a ssf file
data SSFUDG =
      SSFMinus
    | SSFHalf
    | SSFPlus
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show SSFUDG where
    show SSFMinus = "minus"
    show SSFHalf  = "half"
    show SSFPlus  = "plus"

makeSSFUDG :: MonadFail m => String -> m SSFUDG
makeSSFUDG x
    | x == "minus" = pure SSFMinus
    | x == "half"  = pure SSFHalf
    | x == "plus"  = pure SSFPlus
    | otherwise    = fail $ "UDG " ++ show x ++ " not in [minus, half, plus]"

instance Csv.ToField SSFUDG where
    toField x = Csv.toField $ show x
instance Csv.FromField SSFUDG where
    parseField x = Csv.parseField x >>= makeSSFUDG
instance ToJSON SSFUDG where
    toEncoding x = text $ T.pack $ show x
instance FromJSON SSFUDG where
    parseJSON = withText "SSFUDG" (makeSSFUDG . T.unpack)

-- |A datatype to represent Library_Built in a janno file
data SSFLibraryBuilt =
      SSFDS
    | SSFSS
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show SSFLibraryBuilt where
    show SSFDS = "ds"
    show SSFSS = "ss"

makeSSFLibraryBuilt :: MonadFail m => String -> m SSFLibraryBuilt
makeSSFLibraryBuilt x
    | x == "ds"    = pure SSFDS
    | x == "ss"    = pure SSFSS
    | otherwise    = fail $ "Library_Built " ++ show x ++ " not in [ds, ss]"

instance Csv.ToField SSFLibraryBuilt where
    toField x = Csv.toField $ show x
instance Csv.FromField SSFLibraryBuilt where
    parseField x = Csv.parseField x >>= makeSSFLibraryBuilt
instance ToJSON SSFLibraryBuilt where
    toEncoding x = text $ T.pack $ show x
instance FromJSON SSFLibraryBuilt where
    parseJSON = withText "SSFLibraryBuilt" (makeSSFLibraryBuilt . T.unpack)

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

instance ToJSON SeqSourceRows where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SeqSourceRows

-- A data type to represent a run accession ID
newtype AccessionIDRun = AccessionIDRun {getRunAccession :: AccessionID}
    deriving (Eq, Generic)

makeAccessionIDRun :: MonadFail m => String -> m AccessionIDRun
makeAccessionIDRun x = do
    accsID <- makeAccessionID x
    case accsID of
        (INSDCRun y) -> pure $ AccessionIDRun (INSDCRun y)
        _            -> fail $ "Accession " ++ show x ++ " not a correct run accession"

instance Show AccessionIDRun where
    show (AccessionIDRun x) = show x

instance Csv.ToField AccessionIDRun where
    toField x = Csv.toField $ show x
instance Csv.FromField AccessionIDRun where
    parseField x = Csv.parseField x >>= makeAccessionIDRun
instance ToJSON AccessionIDRun where
    toEncoding x = text $ T.pack $ show x
instance FromJSON AccessionIDRun where
    parseJSON = withText "AccessionIDRun" (makeAccessionIDRun . T.unpack)

-- A data type to represent a sample accession ID
newtype AccessionIDSample = AccessionIDSample {getSampleAccession :: AccessionID}
    deriving (Eq, Generic)

makeAccessionIDSample :: MonadFail m => String -> m AccessionIDSample
makeAccessionIDSample x = do
    accsID <- makeAccessionID x
    case accsID of
        (INSDCBioSample y) -> pure $ AccessionIDSample (INSDCBioSample y)
        (INSDCSample y)    -> pure $ AccessionIDSample (INSDCSample y)
        _                  -> fail $ "Accession " ++ show x ++ " not a correct biosample/sample accession"

instance Show AccessionIDSample where
    show (AccessionIDSample x) = show x

instance Csv.ToField AccessionIDSample where
    toField x = Csv.toField $ show x
instance Csv.FromField AccessionIDSample where
    parseField x = Csv.parseField x >>= makeAccessionIDSample
instance ToJSON AccessionIDSample where
    toEncoding x = text $ T.pack $ show x
instance FromJSON AccessionIDSample where
    parseJSON = withText "AccessionIDSample" (makeAccessionIDSample . T.unpack)

-- A data type to represent a study accession ID
newtype AccessionIDStudy = AccessionIDStudy {getStudyAccession :: AccessionID}
    deriving (Eq, Generic)

instance Show AccessionIDStudy where
    show (AccessionIDStudy x) = show x

makeAccessionIDStudy :: MonadFail m => String -> m AccessionIDStudy
makeAccessionIDStudy x = do
    accsID <- makeAccessionID x
    case accsID of
        (INSDCProject y) -> pure $ AccessionIDStudy (INSDCProject y)
        (INSDCStudy y)   -> pure $ AccessionIDStudy (INSDCStudy y)
        _                -> fail $ "Accession " ++ show x ++ " not a correct project/study accession"

instance Csv.ToField AccessionIDStudy where
    toField x = Csv.toField $ show x
instance Csv.FromField AccessionIDStudy where
    parseField x = Csv.parseField x >>= makeAccessionIDStudy
instance ToJSON AccessionIDStudy where
    toEncoding x = text $ T.pack $ show x
instance FromJSON AccessionIDStudy where
    parseJSON = withText "AccessionIDStudy" (makeAccessionIDStudy . T.unpack)

-- | A datatype for calendar dates
newtype SimpleDate = SimpleDate Day
    deriving (Eq, Ord, Generic)

instance Show SimpleDate where
    show (SimpleDate x) = formatTime defaultTimeLocale "%Y-%-m-%-d" x

makeSimpleDate :: MonadFail m => String -> m SimpleDate
makeSimpleDate x = do
    mday <- parseTimeM False defaultTimeLocale "%Y-%-m-%-d" x
    pure (SimpleDate mday)

instance Csv.ToField SimpleDate where
    toField (SimpleDate x) = Csv.toField $ show x
instance Csv.FromField SimpleDate where
    parseField x = Csv.parseField x >>= makeSimpleDate
instance ToJSON SimpleDate where
    toEncoding x = text $ T.pack $ show x
instance FromJSON SimpleDate where
    parseJSON = withText "SimpleDate" (makeSimpleDate . T.unpack)

-- | A datatype to represent MD5 hashes
newtype MD5 = MD5 String
    deriving (Eq, Ord, Generic)

instance Show MD5 where
    show (MD5 x) = x

makeMD5 :: MonadFail m => String -> m MD5
makeMD5 x
    | isMD5Hash x = pure $ MD5 x
    | otherwise   = fail $ "MD5 hash " ++ show x ++ " not well structured"

isMD5Hash :: String -> Bool
isMD5Hash x = length x == 32 && all isHexDigit x

instance Csv.ToField MD5 where
    toField x = Csv.toField $ show x
instance Csv.FromField MD5 where
    parseField x = Csv.parseField x >>= makeMD5
instance ToJSON MD5 where
    toEncoding x = text $ T.pack $ show x
instance FromJSON MD5 where
    parseJSON = withText "MD5" (makeMD5 . T.unpack)

-- | A data type to represent a row in the seqSourceFile
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/seqSourceFile_columns.tsv
-- for more details
data SeqSourceRow = SeqSourceRow
    { sPoseidonID               :: Maybe JannoStringList
    , sUDG                      :: Maybe SSFUDG
    , sLibraryBuilt             :: Maybe SSFLibraryBuilt
    , sSampleAccession          :: Maybe AccessionIDSample
    , sStudyAccession           :: Maybe AccessionIDStudy
    , sRunAccession             :: Maybe AccessionIDRun
    , sSampleAlias              :: Maybe String
    , sSecondarySampleAccession :: Maybe String
    , sFirstPublic              :: Maybe SimpleDate
    , sLastUpdated              :: Maybe SimpleDate
    , sInstrumentModel          :: Maybe String
    , sLibraryLayout            :: Maybe String
    , sLibrarySource            :: Maybe String
    , sInstrumentPlatform       :: Maybe String
    , sLibraryName              :: Maybe String
    , sLibraryStrategy          :: Maybe String
    , sFastqFTP                 :: Maybe (JannoList JURI)
    , sFastqASPERA              :: Maybe (JannoList JURI)
    , sFastqBytes               :: Maybe (JannoList Integer) -- integer, not int, because it can be a very large number
    , sFastqMD5                 :: Maybe (JannoList MD5)
    , sReadCount                :: Maybe Integer             -- integer, not int, because it can be a very large number
    , sSubmittedFTP             :: Maybe (JannoList JURI)
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

instance ToJSON SeqSourceRow where
    toEncoding = genericToEncoding (defaultOptions {omitNothingFields = True})

instance FromJSON SeqSourceRow

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
    toNamedRecord s = Csv.namedRecord [
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
    let seqSourceAsBytestringwithNA = explicitNA seqSourceAsBytestring
    Bch.writeFile path seqSourceAsBytestringwithNA
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
        let consistentSeqSource = checkSeqSourceConsistency seqSourcePath $ SeqSourceRows $ rights seqSourceRepresentation
        case consistentSeqSource of
            Left e  -> do liftIO $ throwIO (e :: PoseidonException)
            Right x -> do
                -- warnings
                warnSeqSourceConsistency seqSourcePath x
                -- finally return the good ones
                return x

-- | A function to read one row of a seqSourceFile
readSeqSourceFileRow :: FilePath -> (Int, Bch.ByteString) -> PoseidonIO (Either PoseidonException SeqSourceRow)
readSeqSourceFileRow seqSourcePath (lineNumber, row) = do
    case Csv.decodeByNameWith decodingOptions row of
        Left e -> do
            let betterError = case P.parse parseCsvParseError "" e of
                    Left _       -> removeUselessSuffix e
                    Right result -> renderCsvParseError result
            return $ Left $ PoseidonFileRowException seqSourcePath (show lineNumber) betterError
        Right (_, seqSourceRow :: V.Vector SeqSourceRow) -> do
            case checkSeqSourceRowConsistency seqSourcePath lineNumber $ V.head seqSourceRow of
                Left e                     -> do return $ Left e
                Right (pS :: SeqSourceRow) -> do return $ Right pS

-- SeqSource consistency checks
checkSeqSourceConsistency :: FilePath -> SeqSourceRows -> Either PoseidonException SeqSourceRows
checkSeqSourceConsistency _ xs
    -- | ... no tests implemented
    | otherwise = Right xs

checkSeqSourceRowConsistency :: FilePath -> Int -> SeqSourceRow -> Either PoseidonException SeqSourceRow
checkSeqSourceRowConsistency _ _ x
    -- | ... no tests implemented
    | otherwise = Right x

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
