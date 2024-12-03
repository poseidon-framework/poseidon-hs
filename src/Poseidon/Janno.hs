{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- the following three are necessary for deriveGeneric
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

module Poseidon.Janno (
    JannoRow(..),
    GeneticSex (..),
    GroupName (..),
    ListColumn (..),
    Sex (..),
    JannoCountryISO (..),
    JannoLatitude (..),
    JannoLongitude (..),
    JannoDateType (..),
    JannoDateBCADMedian (..),
    JannoCaptureType (..),
    JannoGenotypePloidy (..),
    JannoUDG (..),
    JannoRelationDegree (..),
    JannoLibraryBuilt (..),
    writeJannoFile,
    readJannoFile,
    createMinimalJanno,
    createMinimalSample,
    jannoHeaderString,
    CsvNamedRecord (..),
    JannoRows (..),
    JannoStringList,
    filterLookup,
    filterLookupOptional,
    getCsvNR,
    encodingOptions,
    decodingOptions,
    explicitNA,
    removeUselessSuffix,
    parseCsvParseError,
    renderCsvParseError,
    getMaybeListColumn,
    jannoRows2EigenstratIndEntries
) where

import           Poseidon.ColumnTypes
import           Poseidon.Utils                       (PoseidonException (..),
                                                       PoseidonIO, logDebug,
                                                       logError, logWarning,
                                                       renderPoseidonException)

import           Control.Exception                    (throwIO)
import           Control.Monad                        (unless, when)
import qualified Control.Monad.Except                 as E
import           Control.Monad.IO.Class               (liftIO)
import qualified Control.Monad.Writer                 as W
import           Data.Bifunctor                       (second)
import qualified Data.ByteString.Char8                as Bchs
import qualified Data.ByteString.Lazy.Char8           as Bch
import           Data.Char                            (chr, ord)
import qualified Data.Csv                             as Csv
import           Data.Either                          (lefts, rights)
import qualified Data.HashMap.Strict                  as HM
import           Data.List                            (elemIndex, foldl',
                                                       intercalate, nub, sort,
                                                       (\\))
import           Data.Maybe                           (fromJust)
import qualified Data.Text                            as T
import qualified Data.Vector                          as V
import           Generics.SOP.TH                      (deriveGeneric)
import           GHC.Generics                         (Generic)
import           Options.Applicative.Help.Levenshtein (editDistance)
import           SequenceFormats.Eigenstrat           (EigenstratIndEntry (..),
                                                       Sex (..))
import qualified Text.Parsec                          as P
import qualified Text.Parsec.String                   as P

-- | A general datatype for janno list columns
newtype ListColumn a = ListColumn {getListColumn :: [a]}
    deriving (Eq, Ord, Generic, Show)

getMaybeListColumn :: Maybe (ListColumn a) -> [a]
getMaybeListColumn Nothing  = []
getMaybeListColumn (Just x) = getListColumn x

type JannoStringList = ListColumn String

instance (Csv.ToField a, Show a) => Csv.ToField (ListColumn a) where
    toField x = Bchs.intercalate ";" $ map Csv.toField $ getListColumn x
instance (Csv.FromField a) => Csv.FromField (ListColumn a) where
    parseField x = fmap ListColumn . mapM Csv.parseField $ Bchs.splitWith (==';') x

-- | A datatype to collect additional, unpecified .janno file columns (a hashmap in cassava/Data.Csv)
newtype CsvNamedRecord = CsvNamedRecord Csv.NamedRecord deriving (Show, Eq, Generic)

getCsvNR :: CsvNamedRecord -> Csv.NamedRecord
getCsvNR (CsvNamedRecord x) = x

-- | A  data type to represent a janno file
newtype JannoRows = JannoRows {getJannoRows :: [JannoRow]}
    deriving (Show, Eq, Generic)

instance Semigroup JannoRows where
    (JannoRows j1) <> (JannoRows j2) = JannoRows $ j1 `combineTwoJannos` j2
        where
        combineTwoJannos :: [JannoRow] -> [JannoRow] -> [JannoRow]
        combineTwoJannos janno1 janno2 =
            let simpleJannoSum = janno1 ++ janno2
                toAddColNames = HM.keys (HM.unions (map (getCsvNR . jAdditionalColumns) simpleJannoSum))
                toAddEmptyCols = HM.fromList (map (\k -> (k, "n/a")) toAddColNames)
            in map (addEmptyAddColsToJannoRow toAddEmptyCols) simpleJannoSum
        addEmptyAddColsToJannoRow :: Csv.NamedRecord -> JannoRow -> JannoRow
        addEmptyAddColsToJannoRow toAdd x =
            x { jAdditionalColumns = CsvNamedRecord $ fillAddCols toAdd (getCsvNR $ jAdditionalColumns x) }
        fillAddCols :: Csv.NamedRecord -> Csv.NamedRecord -> Csv.NamedRecord
        fillAddCols toAdd cur = HM.union cur (toAdd `HM.difference` cur)

instance Monoid JannoRows where
    mempty = JannoRows []
    mconcat = foldl' mappend mempty

-- | A data type to represent a sample/janno file row
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/janno_columns.tsv
-- for more details
data JannoRow = JannoRow
    { jPoseidonID                 :: String
    , jGeneticSex                 :: GeneticSex
    , jGroupName                  :: ListColumn GroupName
    , jAlternativeIDs             :: Maybe (ListColumn JannoAlternativeID)
    , jRelationTo                 :: Maybe (ListColumn JannoRelationTo)
    , jRelationDegree             :: Maybe (ListColumn JannoRelationDegree)
    , jRelationType               :: Maybe (ListColumn JannoRelationType)
    , jRelationNote               :: Maybe JannoRelationNote
    , jCollectionID               :: Maybe JannoCollectionID
    , jCountry                    :: Maybe JannoCountry
    , jCountryISO                 :: Maybe JannoCountryISO
    , jLocation                   :: Maybe JannoLocation
    , jSite                       :: Maybe JannoSite
    , jLatitude                   :: Maybe JannoLatitude
    , jLongitude                  :: Maybe JannoLongitude
    , jDateType                   :: Maybe JannoDateType
    , jDateC14Labnr               :: Maybe (ListColumn JannoDateC14Labnr)
    , jDateC14UncalBP             :: Maybe (ListColumn JannoDateC14UncalBP)
    , jDateC14UncalBPErr          :: Maybe (ListColumn JannoDateC14UncalBPErr)
    , jDateBCADStart              :: Maybe JannoDateBCADStart
    , jDateBCADMedian             :: Maybe JannoDateBCADMedian
    , jDateBCADStop               :: Maybe JannoDateBCADStop
    , jDateNote                   :: Maybe JannoDateNote
    , jMTHaplogroup               :: Maybe JannoMTHaplogroup
    , jYHaplogroup                :: Maybe JannoYHaplogroup
    , jSourceTissue               :: Maybe (ListColumn JannoSourceTissue)
    , jNrLibraries                :: Maybe JannoNrLibraries
    , jLibraryNames               :: Maybe (ListColumn JannoLibraryName)
    , jCaptureType                :: Maybe (ListColumn JannoCaptureType)
    , jUDG                        :: Maybe JannoUDG
    , jLibraryBuilt               :: Maybe JannoLibraryBuilt
    , jGenotypePloidy             :: Maybe JannoGenotypePloidy
    , jDataPreparationPipelineURL :: Maybe JannoDataPreparationPipelineURL
    , jEndogenous                 :: Maybe JannoEndogenous
    , jNrSNPs                     :: Maybe JannoNrSNPs
    , jCoverageOnTargets          :: Maybe JannoCoverageOnTargets
    , jDamage                     :: Maybe JannoDamage
    , jContamination              :: Maybe (ListColumn JannoContamination)
    , jContaminationErr           :: Maybe (ListColumn JannoContaminationErr)
    , jContaminationMeas          :: Maybe (ListColumn JannoContaminationMeas)
    , jContaminationNote          :: Maybe JannoContaminationNote
    , jGeneticSourceAccessionIDs  :: Maybe (ListColumn JannoGeneticSourceAccessionID)
    , jPrimaryContact             :: Maybe JannoPrimaryContact
    , jPublication                :: Maybe (ListColumn JannoPublication)
    , jComments                   :: Maybe JannoComment
    , jKeywords                   :: Maybe (ListColumn JannoKeyword)
    , jAdditionalColumns          :: CsvNamedRecord
    }
    deriving (Show, Eq, Generic)

-- This header also defines the output column order when writing to csv!
jannoHeader :: [Bchs.ByteString]
jannoHeader = [
      "Poseidon_ID"
    , "Genetic_Sex"
    , "Group_Name"
    , "Alternative_IDs"
    , "Relation_To", "Relation_Degree", "Relation_Type", "Relation_Note"
    , "Collection_ID"
    , "Country", "Country_ISO"
    , "Location", "Site", "Latitude", "Longitude"
    , "Date_Type"
    , "Date_C14_Labnr", "Date_C14_Uncal_BP", "Date_C14_Uncal_BP_Err"
    , "Date_BC_AD_Start", "Date_BC_AD_Median", "Date_BC_AD_Stop"
    , "Date_Note"
    , "MT_Haplogroup", "Y_Haplogroup"
    , "Source_Tissue"
    , "Nr_Libraries", "Library_Names"
    , "Capture_Type", "UDG", "Library_Built", "Genotype_Ploidy"
    , "Data_Preparation_Pipeline_URL"
    , "Endogenous", "Nr_SNPs", "Coverage_on_Target_SNPs", "Damage"
    , "Contamination", "Contamination_Err", "Contamination_Meas", "Contamination_Note"
    , "Genetic_Source_Accession_IDs"
    , "Primary_Contact"
    , "Publication"
    , "Note"
    , "Keywords"
    ]

instance Csv.DefaultOrdered JannoRow where
    headerOrder _ = Csv.header jannoHeader

jannoHeaderString :: [String]
jannoHeaderString = map Bchs.unpack jannoHeader

-- This hashmap represents an empty janno file with all normal, specified columns
jannoRefHashMap :: HM.HashMap Bchs.ByteString ()
jannoRefHashMap = HM.fromList $ map (\x -> (x, ())) jannoHeader

instance Csv.FromNamedRecord JannoRow where
    parseNamedRecord m = JannoRow
        <$> filterLookup         m "Poseidon_ID"
        <*> filterLookup         m "Genetic_Sex"
        <*> filterLookup         m "Group_Name"
        <*> filterLookupOptional m "Alternative_IDs"
        <*> filterLookupOptional m "Relation_To"
        <*> filterLookupOptional m "Relation_Degree"
        <*> filterLookupOptional m "Relation_Type"
        <*> filterLookupOptional m "Relation_Note"
        <*> filterLookupOptional m "Collection_ID"
        <*> filterLookupOptional m "Country"
        <*> filterLookupOptional m "Country_ISO"
        <*> filterLookupOptional m "Location"
        <*> filterLookupOptional m "Site"
        <*> filterLookupOptional m "Latitude"
        <*> filterLookupOptional m "Longitude"
        <*> filterLookupOptional m "Date_Type"
        <*> filterLookupOptional m "Date_C14_Labnr"
        <*> filterLookupOptional m "Date_C14_Uncal_BP"
        <*> filterLookupOptional m "Date_C14_Uncal_BP_Err"
        <*> filterLookupOptional m "Date_BC_AD_Start"
        <*> filterLookupOptional m "Date_BC_AD_Median"
        <*> filterLookupOptional m "Date_BC_AD_Stop"
        <*> filterLookupOptional m "Date_Note"
        <*> filterLookupOptional m "MT_Haplogroup"
        <*> filterLookupOptional m "Y_Haplogroup"
        <*> filterLookupOptional m "Source_Tissue"
        <*> filterLookupOptional m "Nr_Libraries"
        <*> filterLookupOptional m "Library_Names"
        <*> filterLookupOptional m "Capture_Type"
        <*> filterLookupOptional m "UDG"
        <*> filterLookupOptional m "Library_Built"
        <*> filterLookupOptional m "Genotype_Ploidy"
        <*> filterLookupOptional m "Data_Preparation_Pipeline_URL"
        <*> filterLookupOptional m "Endogenous"
        <*> filterLookupOptional m "Nr_SNPs"
        <*> filterLookupOptional m "Coverage_on_Target_SNPs"
        <*> filterLookupOptional m "Damage"
        <*> filterLookupOptional m "Contamination"
        <*> filterLookupOptional m "Contamination_Err"
        <*> filterLookupOptional m "Contamination_Meas"
        <*> filterLookupOptional m "Contamination_Note"
        <*> filterLookupOptional m "Genetic_Source_Accession_IDs"
        <*> filterLookupOptional m "Primary_Contact"
        <*> filterLookupOptional m "Publication"
        <*> filterLookupOptional m "Note"
        <*> filterLookupOptional m "Keywords"
        -- beyond that read everything that is not in the set of defined variables
        -- as a separate hashmap
        <*> pure (CsvNamedRecord (m `HM.difference` jannoRefHashMap))

filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = case cleanInput $ HM.lookup name m of
    Nothing -> fail "Missing value in mandatory column (Poseidon_ID, Genetic_Sex, Group_Name)"
    Just x  -> Csv.parseField  x

filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) Csv.parseField . cleanInput $ HM.lookup name m

cleanInput :: Maybe Bchs.ByteString -> Maybe Bchs.ByteString
cleanInput Nothing           = Nothing
cleanInput (Just rawInputBS) = transNA rawInputBS
    where
        transNA :: Bchs.ByteString -> Maybe Bchs.ByteString
        transNA ""    = Nothing
        transNA "n/a" = Nothing
        transNA x     = Just x

instance Csv.ToNamedRecord JannoRow where
    toNamedRecord j = Csv.namedRecord [
          "Poseidon_ID"                     Csv..= jPoseidonID j
        , "Genetic_Sex"                     Csv..= jGeneticSex j
        , "Group_Name"                      Csv..= jGroupName j
        , "Alternative_IDs"                 Csv..= jAlternativeIDs j
        , "Relation_To"                     Csv..= jRelationTo j
        , "Relation_Degree"                 Csv..= jRelationDegree j
        , "Relation_Type"                   Csv..= jRelationType j
        , "Relation_Note"                   Csv..= jRelationNote j
        , "Collection_ID"                   Csv..= jCollectionID j
        , "Country"                         Csv..= jCountry j
        , "Country_ISO"                     Csv..= jCountryISO j
        , "Location"                        Csv..= jLocation j
        , "Site"                            Csv..= jSite j
        , "Latitude"                        Csv..= jLatitude j
        , "Longitude"                       Csv..= jLongitude j
        , "Date_Type"                       Csv..= jDateType j
        , "Date_C14_Labnr"                  Csv..= jDateC14Labnr j
        , "Date_C14_Uncal_BP"               Csv..= jDateC14UncalBP j
        , "Date_C14_Uncal_BP_Err"           Csv..= jDateC14UncalBPErr j
        , "Date_BC_AD_Start"                Csv..= jDateBCADStart j
        , "Date_BC_AD_Median"               Csv..= jDateBCADMedian j
        , "Date_BC_AD_Stop"                 Csv..= jDateBCADStop j
        , "Date_Note"                       Csv..= jDateNote j
        , "MT_Haplogroup"                   Csv..= jMTHaplogroup j
        , "Y_Haplogroup"                    Csv..= jYHaplogroup j
        , "Source_Tissue"                   Csv..= jSourceTissue j
        , "Nr_Libraries"                    Csv..= jNrLibraries j
        , "Library_Names"                   Csv..= jLibraryNames j
        , "Capture_Type"                    Csv..= jCaptureType j
        , "UDG"                             Csv..= jUDG j
        , "Library_Built"                   Csv..= jLibraryBuilt j
        , "Genotype_Ploidy"                 Csv..= jGenotypePloidy j
        , "Data_Preparation_Pipeline_URL"   Csv..= jDataPreparationPipelineURL j
        , "Endogenous"                      Csv..= jEndogenous j
        , "Nr_SNPs"                         Csv..= jNrSNPs j
        , "Coverage_on_Target_SNPs"         Csv..= jCoverageOnTargets j
        , "Damage"                          Csv..= jDamage j
        , "Contamination"                   Csv..= jContamination j
        , "Contamination_Err"               Csv..= jContaminationErr j
        , "Contamination_Meas"              Csv..= jContaminationMeas j
        , "Contamination_Note"              Csv..= jContaminationNote j
        , "Genetic_Source_Accession_IDs"    Csv..= jGeneticSourceAccessionIDs j
        , "Primary_Contact"                 Csv..= jPrimaryContact j
        , "Publication"                     Csv..= jPublication j
        , "Note"                            Csv..= jComments j
        , "Keywords"                        Csv..= jKeywords j
        -- beyond that add what is in the hashmap of additional columns
        ] `HM.union` (getCsvNR $ jAdditionalColumns j)

-- | A function to create empty janno rows for a set of individuals
createMinimalJanno :: [EigenstratIndEntry] -> JannoRows
createMinimalJanno [] = mempty
createMinimalJanno xs = JannoRows $ map createMinimalSample xs

-- | A function to create an empty janno row for an individual
createMinimalSample :: EigenstratIndEntry -> JannoRow
createMinimalSample (EigenstratIndEntry id_ sex pop) =
    JannoRow {
          jPoseidonID                   = id_
        , jGeneticSex                   = GeneticSex sex
        , jGroupName                    = ListColumn [GroupName $ T.pack pop]
        , jAlternativeIDs               = Nothing
        , jRelationTo                   = Nothing
        , jRelationDegree               = Nothing
        , jRelationType                 = Nothing
        , jRelationNote                 = Nothing
        , jCollectionID                 = Nothing
        , jCountry                      = Nothing
        , jCountryISO                   = Nothing
        , jLocation                     = Nothing
        , jSite                         = Nothing
        , jLatitude                     = Nothing
        , jLongitude                    = Nothing
        , jDateType                     = Nothing
        , jDateC14Labnr                 = Nothing
        , jDateC14UncalBP               = Nothing
        , jDateC14UncalBPErr            = Nothing
        , jDateBCADStart                = Nothing
        , jDateBCADMedian               = Nothing
        , jDateBCADStop                 = Nothing
        , jDateNote                     = Nothing
        , jMTHaplogroup                 = Nothing
        , jYHaplogroup                  = Nothing
        , jSourceTissue                 = Nothing
        , jNrLibraries                  = Nothing
        , jLibraryNames                 = Nothing
        , jCaptureType                  = Nothing
        , jUDG                          = Nothing
        , jLibraryBuilt                 = Nothing
        , jGenotypePloidy               = Nothing
        , jDataPreparationPipelineURL   = Nothing
        , jEndogenous                   = Nothing
        , jNrSNPs                       = Nothing
        , jCoverageOnTargets            = Nothing
        , jDamage                       = Nothing
        , jContamination                = Nothing
        , jContaminationErr             = Nothing
        , jContaminationMeas            = Nothing
        , jContaminationNote            = Nothing
        , jGeneticSourceAccessionIDs    = Nothing
        , jPrimaryContact               = Nothing
        , jPublication                  = Nothing
        , jComments                     = Nothing
        , jKeywords                     = Nothing
        -- The template should of course not have any additional columns
        , jAdditionalColumns            = CsvNamedRecord $ HM.fromList []
    }

-- Janno file writing

writeJannoFile :: FilePath -> JannoRows -> IO ()
writeJannoFile path (JannoRows rows) = do
    let jannoAsBytestring = Csv.encodeByNameWith encodingOptions makeHeaderWithAdditionalColumns rows
    let jannoAsBytestringwithNA = explicitNA jannoAsBytestring
    Bch.writeFile path jannoAsBytestringwithNA
    where
        makeHeaderWithAdditionalColumns :: Csv.Header
        makeHeaderWithAdditionalColumns =
            V.fromList $ jannoHeader ++ sort (HM.keys (HM.unions (map (getCsvNR . jAdditionalColumns) rows)))

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    , Csv.encUseCrLf = False
    , Csv.encIncludeHeader = True
    , Csv.encQuoting = Csv.QuoteMinimal
}

-- | A function to load one janno file
readJannoFile :: FilePath -> PoseidonIO JannoRows
readJannoFile jannoPath = do
    logDebug $ "Reading: " ++ jannoPath
    jannoFile <- liftIO $ Bch.readFile jannoPath
    let jannoFileRows = Bch.lines jannoFile
    when (length jannoFileRows < 2) $ liftIO $ throwIO $ PoseidonFileConsistencyException jannoPath "File has less than two lines"
    logDebug $ show (length jannoFileRows - 1) ++ " samples in this file"
    -- tupel with row number and row bytestring
    let jannoFileRowsWithNumber = zip [1..(length jannoFileRows)] jannoFileRows
    -- filter out empty lines
        jannoFileRowsWithNumberFiltered = filter (\(_, y) -> y /= Bch.empty) jannoFileRowsWithNumber
    -- create header + individual line combination
        headerOnlyPotentiallyWithQuotes = snd $ head jannoFileRowsWithNumberFiltered
        -- removing the quotes like this might cause issues in edge cases
        headerOnly = Bch.filter (/= '"') headerOnlyPotentiallyWithQuotes
        rowsOnly = tail jannoFileRowsWithNumberFiltered
        jannoFileRowsWithHeader = map (second (\x -> headerOnly <> "\n" <> x)) rowsOnly
    -- report missing or additional columns
    let jannoColNames = map Bch.toStrict (Bch.split '\t' headerOnly)
        missing_columns = map Bchs.unpack $ jannoHeader \\ jannoColNames
        additional_columns = map Bchs.unpack $ jannoColNames \\ jannoHeader
    --unless (null missing_columns) $ do
    --    logDebug ("Missing standard columns: " ++ intercalate ", " missing_columns)
    unless (null additional_columns) $ do
        logDebug ("Additional columns: " ++
        -- for each additional column a standard column is suggested: "Countro (Country?)"
            intercalate ", " (zipWith (\x y -> x ++ " (" ++ y ++ "?)")
            additional_columns (findSimilarNames missing_columns additional_columns)))
    -- load janno by rows
    jannoRepresentation <- mapM (readJannoFileRow jannoPath) jannoFileRowsWithHeader
    -- error case management
    if not (null (lefts jannoRepresentation))
    then do
        mapM_ (logError . renderPoseidonException) $ take 5 $ lefts jannoRepresentation
        liftIO $ throwIO $ PoseidonFileConsistencyException jannoPath "Broken lines."
    else do
        let consistentJanno = checkJannoConsistency jannoPath $ JannoRows $ rights jannoRepresentation
        case consistentJanno of
            Left e -> do liftIO $ throwIO e
            Right x -> do
                -- putStrLn ""
                -- putStrLn $ show $ map jSourceTissue x
                -- putStrLn $ show $ map jLongitude x
                -- putStrLn $ show $ map jUDG x
                return x

findSimilarNames :: [String] -> [String] -> [String]
findSimilarNames reference = map (findSimilar reference)
    where
        findSimilar ::  [String] -> String -> String
        findSimilar [] _  = []
        findSimilar ref x =
            let dists = map (\y -> x `editDistance` y) ref
            in ref !! fromJust (elemIndex (minimum dists) dists)

-- | A function to load one row of a janno file
readJannoFileRow :: FilePath -> (Int, Bch.ByteString) -> PoseidonIO (Either PoseidonException JannoRow)
readJannoFileRow jannoPath (lineNumber, row) = do
    let decoded = Csv.decodeByNameWith decodingOptions row
        simplifiedDecoded = (\(_,rs) -> V.head rs) <$> decoded
    case simplifiedDecoded of
        Left e -> do
            let betterError = case P.parse parseCsvParseError "" e of
                    Left _       -> removeUselessSuffix e
                    Right result -> renderCsvParseError result
            return $ Left $ PoseidonFileRowException jannoPath (show lineNumber) betterError
        Right jannoRow -> do
            let (errOrJannoRow, warnings) = W.runWriter (E.runExceptT (checkJannoRowConsistency jannoRow))
            mapM_ (logWarning . renderWarning) warnings
            case errOrJannoRow of
                Left e  -> return $ Left $ PoseidonFileRowException jannoPath renderLocation e
                Right r -> return $ Right r
            where
                renderWarning :: String -> String
                renderWarning e = "Issue in " ++ jannoPath ++ " " ++
                                  "in line " ++ renderLocation ++ ": " ++ e
                renderLocation :: String
                renderLocation =  show lineNumber ++
                                  " (Poseidon_ID: " ++ jPoseidonID jannoRow ++ ")"

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

removeUselessSuffix :: String -> String
removeUselessSuffix = T.unpack . T.replace " at \"\"" "" . T.pack

-- reformat the parser error
-- from: parse error (Failed reading: conversion error: expected Int, got "430;" (incomplete field parse, leftover: [59]))
-- to:   parse error in one column (expected data type: Int, broken value: "430;", problematic characters: ";")

data CsvParseError = CsvParseError {
      _expected :: String
    , _actual   :: String
    , _leftover :: String
} deriving Show

parseCsvParseError :: P.Parser CsvParseError
parseCsvParseError = do
    _ <- P.string "parse error (Failed reading: conversion error: expected "
    expected <- P.manyTill P.anyChar (P.try (P.string ", got "))
    actual <- P.manyTill P.anyChar (P.try (P.string " (incomplete field parse, leftover: ["))
    leftoverList <- P.sepBy (read <$> P.many1 P.digit) (P.char ',')
    _ <- P.char ']'
    _ <- P.many P.anyChar
    return $ CsvParseError expected actual (map chr leftoverList)

renderCsvParseError :: CsvParseError -> String
renderCsvParseError (CsvParseError expected actual leftover) =
    "parse error in one column (" ++
    "expected data type: " ++ expected ++ ", " ++
    "broken value: " ++ actual ++ ", " ++
    "problematic characters: " ++ show leftover ++ ")"

-- | A helper functions to replace empty bytestrings values in janno files with explicit "n/a"
explicitNA :: Bch.ByteString -> Bch.ByteString
explicitNA = replaceInJannoBytestring Bch.empty "n/a"

replaceInJannoBytestring :: Bch.ByteString -> Bch.ByteString -> Bch.ByteString -> Bch.ByteString
replaceInJannoBytestring from to tsv =
    let tsvRows = Bch.lines tsv
        tsvCells = map (Bch.splitWith (=='\t')) tsvRows
        tsvCellsUpdated = map (map (\y -> if y == from || y == Bch.append from "\r" then to else y)) tsvCells
        tsvRowsUpdated = map (Bch.intercalate (Bch.pack "\t")) tsvCellsUpdated
   in Bch.unlines tsvRowsUpdated

-- Global janno consistency checks

checkJannoConsistency :: FilePath -> JannoRows -> Either PoseidonException JannoRows
checkJannoConsistency jannoPath xs
    | not $ checkIndividualUnique xs = Left $ PoseidonFileConsistencyException jannoPath
        "The Poseidon_IDs are not unique"
    | otherwise = Right xs

checkIndividualUnique :: JannoRows -> Bool
checkIndividualUnique (JannoRows rows) = length rows == length (nub $ map jPoseidonID rows)

-- Row-wise janno consistency checks

type JannoRowWarnings = [String]
type JannoRowLog = E.ExceptT String (W.Writer JannoRowWarnings)

checkJannoRowConsistency :: JannoRow -> JannoRowLog JannoRow
checkJannoRowConsistency x =
    return x
    >>= checkMandatoryStringNotEmpty
    >>= checkC14ColsConsistent
    >>= checkContamColsConsistent
    >>= checkRelationColsConsistent

checkMandatoryStringNotEmpty :: JannoRow -> JannoRowLog JannoRow
checkMandatoryStringNotEmpty x =
    let notEmpty = (not . null . jPoseidonID $ x) &&
                   (not . null . getListColumn . jGroupName $ x) &&
                   (not . null . show . head . getListColumn . jGroupName $ x)
    in case notEmpty of
        False -> E.throwError "Poseidon_ID or Group_Name are empty"
        True  -> return x

getCellLength :: Maybe (ListColumn a) -> Int
getCellLength = maybe 0 (length . getListColumn)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual x  = length (nub x) == 1

checkC14ColsConsistent :: JannoRow -> JannoRowLog JannoRow
checkC14ColsConsistent x =
    let isTypeC14        = jDateType x == Just C14
        lLabnr           = getCellLength $ jDateC14Labnr x
        lUncalBP         = getCellLength $ jDateC14UncalBP x
        lUncalBPErr      = getCellLength $ jDateC14UncalBPErr x
        anyMainColFilled = lUncalBP > 0 || lUncalBPErr > 0
        anyMainColEmpty  = lUncalBP == 0 || lUncalBPErr == 0
        allSameLength    = allEqual [lLabnr, lUncalBP, lUncalBPErr] ||
                          (lLabnr == 0 && lUncalBP == lUncalBPErr)
    in case (isTypeC14, anyMainColFilled, anyMainColEmpty, allSameLength) of
        (False, False, _, _ )   -> return x
        (False, True, _, _ )    -> E.throwError "Date_Type is not \"C14\", but either \
                                         \Date_C14_Uncal_BP or Date_C14_Uncal_BP_Err are not empty"
        (True, _, False, False) -> E.throwError "Date_C14_Labnr, Date_C14_Uncal_BP and Date_C14_Uncal_BP_Err \
                                         \do not have the same lengths"
        (True, _, False, True ) -> return x
        -- this should be an error, but we have legacy packages with this issue, so it's only a warning
        (True, _, True, _ )     -> do
            W.tell ["Date_Type is \"C14\", but either Date_C14_Uncal_BP or Date_C14_Uncal_BP_Err are empty"]
            return x

checkContamColsConsistent :: JannoRow -> JannoRowLog JannoRow
checkContamColsConsistent x =
    let lContamination      = getCellLength $ jContamination x
        lContaminationErr   = getCellLength $ jContaminationErr x
        lContaminationMeas  = getCellLength $ jContaminationMeas x
        allSameLength       = allEqual [lContamination, lContaminationErr, lContaminationMeas]
    in case allSameLength of
        False -> E.throwError "Contamination, Contamination_Err and Contamination_Meas \
                      \do not have the same lengths"
        True  -> return x

checkRelationColsConsistent :: JannoRow -> JannoRowLog JannoRow
checkRelationColsConsistent x =
    let lRelationTo     = getCellLength $ jRelationTo x
        lRelationDegree = getCellLength $ jRelationDegree x
        lRelationType   = getCellLength $ jRelationType x
        allSameLength   = allEqual [lRelationTo, lRelationDegree, lRelationType] ||
                          (allEqual [lRelationTo, lRelationDegree] && lRelationType == 0)
    in case allSameLength of
        False -> E.throwError "Relation_To, Relation_Degree and Relation_Type \
                      \do not have the same lengths. Relation_Type can be empty"
        True  -> return x

-- deriving with TemplateHaskell necessary for the generics magic in the Survey module
deriveGeneric ''JannoRow

-- | a convenience function to construct Eigenstrat Ind entries out of jannoRows
jannoRows2EigenstratIndEntries :: JannoRows -> [EigenstratIndEntry]
jannoRows2EigenstratIndEntries (JannoRows jannoRows) = do -- list monad
    jannoRow <- jannoRows -- looping over jannoRows
    let GroupName gText = head . getListColumn . jGroupName $ jannoRow
    return $ EigenstratIndEntry (jPoseidonID jannoRow) (sfSex (jGeneticSex jannoRow)) (T.unpack gText)
