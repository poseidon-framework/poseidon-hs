{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.Janno (
    JannoRow(..),
    JannoSex (..),
    Sex (..),
    Latitude (..),
    Longitude (..),
    JannoDateType (..),
    JannoDataType (..),
    JannoGenotypePloidy (..),
    Percent (..),
    JannoUDG (..),
    JannoLibraryBuilt (..),
    writeJannoFile,
    readJannoFile,
    createMinimalJanno
) where

import           Poseidon.Utils             (PoseidonException (..),
                                             renderPoseidonException)


import           Control.Applicative        (empty, optional)
import           Control.Exception          (throwIO, try)
import           Control.Monad              (unless, when)
import           Data.Aeson                 (FromJSON, Options (..), ToJSON, Value(..),
                                             defaultOptions, genericToEncoding, toJSON, parseJSON,
                                             toEncoding)
import           Data.Bifunctor             (second)
import qualified Data.ByteString.Char8      as Bchs
import qualified Data.ByteString.Lazy.Char8 as Bch
import           Data.Char                  (ord)
import qualified Data.Csv                   as Csv
import           Data.Either                (isLeft, isRight, lefts, rights)
import           Data.Either.Combinators    (rightToMaybe)
import qualified Data.HashMap.Lazy          as HM
import           Data.List                  (intercalate, nub, (\\))
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..), Sex (..))
import           System.Directory           (doesFileExist)
import           System.IO                  (hPutStrLn, stderr)

newtype JannoSex = JannoSex { sfSex :: Sex }
    deriving (Eq)

instance Ord JannoSex where
    compare (JannoSex Female) (JannoSex Male)    = GT
    compare (JannoSex Male) (JannoSex Female)    = LT
    compare (JannoSex Male) (JannoSex Unknown)   = GT
    compare (JannoSex Unknown) (JannoSex Male)   = LT
    compare (JannoSex Female) (JannoSex Unknown) = GT
    compare (JannoSex Unknown) (JannoSex Female) = LT
    compare _ _            = EQ

instance Csv.FromField JannoSex where
    parseField x
        | x == "F" = pure (JannoSex Female)
        | x == "M" = pure (JannoSex Male)
        | x == "U" = pure (JannoSex Unknown)
        | otherwise = empty

instance Csv.ToField JannoSex where
    toField (JannoSex Female)  = "F"
    toField (JannoSex Male)    = "M"
    toField (JannoSex Unknown) = "U"

instance FromJSON JannoSex where
    parseJSON (String "F") = pure (JannoSex Female)
    parseJSON (String "M") = pure (JannoSex Male)
    parseJSON (String "U") = pure (JannoSex Unknown)
    parseJSON v = fail ("could not parse " ++ show v ++ " as JannoSex")

instance ToJSON JannoSex where
    -- this encodes directly to a bytestring Builder
    toJSON (JannoSex Female)  = String "F"
    toJSON (JannoSex Male)    = String "M"
    toJSON (JannoSex Unknown) = String "U"

instance Show JannoSex where
    show (JannoSex Female)  = "F"
    show (JannoSex Male)    = "M"
    show (JannoSex Unknown) = "U"

-- |A datatype to represent Date_Type in a janno file
data JannoDateType = C14
    | Contextual
    | Modern
    deriving (Eq, Ord, Generic)

instance ToJSON JannoDateType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JannoDateType

instance Csv.FromField JannoDateType where
    parseField x
        | x == "C14" = pure C14
        | x == "contextual" = pure Contextual
        | x == "modern" = pure Modern
        | otherwise = empty

instance Csv.ToField JannoDateType where
    toField C14        = "C14"
    toField Contextual = "contextual"
    toField Modern     = "modern"

instance Show JannoDateType where
    show C14        = "C14"
    show Contextual = "contextual"
    show Modern     = "modern"

-- |A datatype to represent Data_Type in a janno file
data JannoDataType = Shotgun
    | A1240K
    | OtherCapture
    | ReferenceGenome
    deriving (Eq, Ord, Generic)

instance ToJSON JannoDataType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JannoDataType

instance Csv.FromField JannoDataType where
    parseField x
        | x == "Shotgun" = pure Shotgun
        | x == "1240K" = pure A1240K
        | x == "OtherCapture" = pure OtherCapture
        | x == "ReferenceGenome" = pure ReferenceGenome
        | otherwise = empty

instance Csv.ToField JannoDataType where
    toField Shotgun         = "Shotgun"
    toField A1240K          = "1240K"
    toField OtherCapture    = "OtherCapture"
    toField ReferenceGenome = "ReferenceGenome"

instance Show JannoDataType where
    show Shotgun         = "Shotgun"
    show A1240K          = "1240K"
    show OtherCapture    = "OtherCapture"
    show ReferenceGenome = "ReferenceGenome"

-- |A datatype to represent Genotype_Ploidy in a janno file
data JannoGenotypePloidy = Diploid
    | Haploid
    deriving (Eq, Ord, Generic)

instance ToJSON JannoGenotypePloidy where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JannoGenotypePloidy

instance Csv.FromField JannoGenotypePloidy where
    parseField x
        | x == "diploid" = pure Diploid
        | x == "haploid" = pure Haploid
        | otherwise = empty

instance Csv.ToField JannoGenotypePloidy where
    toField Diploid = "diploid"
    toField Haploid = "haploid"

instance Show JannoGenotypePloidy where
    show Diploid = "diploid"
    show Haploid = "haploid"

-- |A datatype to represent UDG in a janno file
data JannoUDG = Minus
    | Half
    | Plus
    | Mixed
    deriving (Eq, Ord, Generic)

instance ToJSON JannoUDG where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JannoUDG

instance Csv.FromField JannoUDG where
    parseField x
        | x == "minus" = pure Minus
        | x == "half" = pure Half
        | x == "plus" = pure Plus
        | x == "mixed" = pure Mixed
        | otherwise = empty

instance Csv.ToField JannoUDG where
    toField Minus = "minus"
    toField Half  = "half"
    toField Plus  = "plus"
    toField Mixed = "mixed"

instance Show JannoUDG where
    show Minus = "minus"
    show Half  = "half"
    show Plus  = "plus"
    show Mixed = "mixed"

-- |A datatype to represent Library_Built in a janno file
data JannoLibraryBuilt = DS
    | SS
    | Other
    deriving (Eq, Ord, Generic)

instance Csv.FromField JannoLibraryBuilt where
    parseField x
        | x == "ds" = pure DS
        | x == "ss" = pure SS
        | x == "other" = pure Other
        | otherwise = empty

instance Csv.ToField JannoLibraryBuilt where
    toField DS    = "ds"
    toField SS    = "ss"
    toField Other = "other"

instance Show JannoLibraryBuilt where
    show DS    = "ds"
    show SS    = "ss"
    show Other = "other"

instance ToJSON JannoLibraryBuilt where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JannoLibraryBuilt

-- | A datatype for Latitudes
newtype Latitude =
        Latitude Double
    deriving (Eq, Ord, Generic)

instance ToJSON Latitude where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Latitude

instance Csv.FromField Latitude where
    parseField x = do
        val <- Csv.parseField x
        if val < -90 || val > 90
        then empty
        else pure (Latitude val)

instance Csv.ToField Latitude where
    toField (Latitude x) = Csv.toField x

instance Show Latitude where
    show (Latitude x) = show x

-- | A datatype for Longitudes
newtype Longitude =
        Longitude Double
    deriving (Eq, Ord, Generic)

instance ToJSON Longitude where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Longitude

instance Csv.FromField Longitude where
    parseField x = do
        val <- Csv.parseField x
        if val < -180 || val > 180
        then empty
        else pure (Longitude val)

instance Csv.ToField Longitude where
    toField (Longitude x) = Csv.toField x

instance Show Longitude where
    show (Longitude x) = show x

-- | A datatype for Percent values
newtype Percent =
        Percent Double
    deriving (Eq, Ord, Generic)

instance ToJSON Percent where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Percent

instance Csv.FromField Percent where
    parseField x = do
        val <- Csv.parseField x
        if val < 0  || val > 100
        then empty
        else pure (Percent val)

instance Csv.ToField Percent where
    toField (Percent x) = Csv.toField x

instance Show Percent where
    show (Percent x) = show x

-- | A data type to represent a sample/janno file row
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/janno_columns.tsv
-- for more details
data JannoRow = JannoRow
    { jIndividualID      :: String
    , jCollectionID      :: Maybe String
    , jSourceTissue      :: Maybe [String]
    , jCountry           :: Maybe String
    , jLocation          :: Maybe String
    , jSite              :: Maybe String
    , jLatitude          :: Maybe Latitude
    , jLongitude         :: Maybe Longitude
    , jDateC14Labnr      :: Maybe [String]
    , jDateC14UncalBP    :: Maybe [Int]
    , jDateC14UncalBPErr :: Maybe [Int]
    , jDateBCADMedian    :: Maybe Int
    , jDateBCADStart     :: Maybe Int
    , jDateBCADStop      :: Maybe Int
    , jDateType          :: Maybe JannoDateType
    , jNrLibraries       :: Maybe Int
    , jDataType          :: Maybe [JannoDataType]
    , jGenotypePloidy    :: Maybe JannoGenotypePloidy
    , jGroupName         :: [String]
    , jGeneticSex        :: JannoSex
    , jNrAutosomalSNPs   :: Maybe Int
    , jCoverage1240K     :: Maybe Double
    , jMTHaplogroup      :: Maybe String
    , jYHaplogroup       :: Maybe String
    , jEndogenous        :: Maybe Percent
    , jUDG               :: Maybe JannoUDG
    , jLibraryBuilt      :: Maybe JannoLibraryBuilt
    , jDamage            :: Maybe Percent
    , jNuclearContam     :: Maybe Double
    , jNuclearContamErr  :: Maybe Double
    , jMTContam          :: Maybe Double
    , jMTContamErr       :: Maybe Double
    , jPrimaryContact    :: Maybe String
    , jPublication       :: Maybe String
    , jComments          :: Maybe String
    , jKeywords          :: Maybe [String]
    }
    deriving (Show, Eq, Generic)

instance ToJSON JannoRow where
    toEncoding = genericToEncoding (defaultOptions {omitNothingFields = True})

instance FromJSON JannoRow

instance Csv.FromNamedRecord JannoRow where
    parseNamedRecord m = pure JannoRow
        <*> filterLookup m "Individual_ID"
        <*> filterLookupOptional m "Collection_ID"
        <*> filterLookupOptional m "Source_Tissue"
        <*> filterLookupOptional m "Country"
        <*> filterLookupOptional m "Location"
        <*> filterLookupOptional m "Site"
        <*> filterLookupOptional m "Latitude"
        <*> filterLookupOptional m "Longitude"
        <*> filterLookupOptional m "Date_C14_Labnr"
        <*> filterLookupOptional m "Date_C14_Uncal_BP"
        <*> filterLookupOptional m "Date_C14_Uncal_BP_Err"
        <*> filterLookupOptional m "Date_BC_AD_Median"
        <*> filterLookupOptional m "Date_BC_AD_Start"
        <*> filterLookupOptional m "Date_BC_AD_Stop"
        <*> filterLookupOptional m "Date_Type"
        <*> filterLookupOptional m "No_of_Libraries"
        <*> filterLookupOptional m "Data_Type"
        <*> filterLookupOptional m "Genotype_Ploidy"
        <*> filterLookup m "Group_Name"
        <*> filterLookup m "Genetic_Sex"
        <*> filterLookupOptional m "Nr_autosomal_SNPs"
        <*> filterLookupOptional m "Coverage_1240K"
        <*> filterLookupOptional m "MT_Haplogroup"
        <*> filterLookupOptional m "Y_Haplogroup"
        <*> filterLookupOptional m "Endogenous"
        <*> filterLookupOptional m "UDG"
        <*> filterLookupOptional m "Library_Built"
        <*> filterLookupOptional m "Damage"
        <*> filterLookupOptional m "Xcontam"
        <*> filterLookupOptional m "Xcontam_stderr"
        <*> filterLookupOptional m "mtContam"
        <*> filterLookupOptional m "mtContam_stderr"
        <*> filterLookupOptional m "Primary_Contact"
        <*> filterLookupOptional m "Publication_Status"
        <*> filterLookupOptional m "Note"
        <*> filterLookupOptional m "Keywords"

filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField . ignoreNA $ HM.lookup name m

filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = case HM.lookup name m of
    Nothing -> pure Nothing
    justField -> case ignoreNA justField of
        Nothing -> pure Nothing
        Just x  -> Csv.parseField x

ignoreNA :: Maybe Bchs.ByteString -> Maybe Bchs.ByteString
ignoreNA (Just "")    = Nothing
ignoreNA (Just "n/a") = Nothing
ignoreNA (Just x)     = Just x
ignoreNA Nothing      = Nothing

instance Csv.ToNamedRecord JannoRow where
    toNamedRecord (JannoRow jIndividualID jCollectionID jSourceTissue jCountry
        jLocation jSite jLatitude jLongitude jDateC14Labnr jDateC14UncalBP
        jDateC14UncalBPErr jDateBCADMedian jDateBCADStart jDateBCADStop jDateType
        jNrLibraries jDataType jGenotypePloidy jGroupName jGeneticSex
        jNrAutosomalSNPs jCoverage1240K jMTHaplogroup jYHaplogroup jEndogenous
        jUDG jLibraryBuilt jDamage jNuclearContam jNuclearContamErr jMTContam
        jMTContamErr jPrimaryContact jPublication jComments jKeywords) = Csv.namedRecord [
          "Individual_ID"           Csv..= jIndividualID
        , "Collection_ID"           Csv..= jCollectionID
        , "Source_Tissue"           Csv..= jSourceTissue
        , "Country"                 Csv..= jCountry
        , "Location"                Csv..= jLocation
        , "Site"                    Csv..= jSite
        , "Latitude"                Csv..= jLatitude
        , "Longitude"               Csv..= jLongitude
        , "Date_C14_Labnr"          Csv..= jDateC14UncalBP
        , "Date_C14_Uncal_BP"       Csv..= jDateC14UncalBP
        , "Date_C14_Uncal_BP_Err"   Csv..= jDateC14UncalBPErr
        , "Date_BC_AD_Median"       Csv..= jDateBCADMedian
        , "Date_BC_AD_Start"        Csv..= jDateBCADStart
        , "Date_BC_AD_Stop"         Csv..= jDateBCADStop
        , "Date_Type"               Csv..= jDateType
        , "No_of_Libraries"         Csv..= jNrLibraries
        , "Data_Type"               Csv..= jDataType
        , "Genotype_Ploidy"         Csv..= jGenotypePloidy
        , "Group_Name"              Csv..= jGroupName
        , "Genetic_Sex"             Csv..= jGeneticSex
        , "Nr_autosomal_SNPs"       Csv..= jNrAutosomalSNPs
        , "Coverage_1240K"          Csv..= jCoverage1240K
        , "MT_Haplogroup"           Csv..= jMTHaplogroup
        , "Y_Haplogroup"            Csv..= jYHaplogroup
        , "Endogenous"              Csv..= jEndogenous
        , "UDG"                     Csv..= jUDG
        , "Library_Built"           Csv..= jLibraryBuilt
        , "Damage"                  Csv..= jDamage
        , "Xcontam"                 Csv..= jNuclearContam
        , "Xcontam_stderr"          Csv..= jNuclearContamErr
        , "mtContam"                Csv..= jMTContam
        , "mtContam_stderr"         Csv..= jMTContamErr
        , "Primary_Contact"         Csv..= jPrimaryContact
        , "Publication_Status"      Csv..= jPublication
        , "Note"                    Csv..= jComments
        , "Keywords"                Csv..= jKeywords
        ]

instance Csv.DefaultOrdered JannoRow where
    headerOrder _ = Csv.header jannoHeader

jannoHeader :: [Bchs.ByteString]
jannoHeader = ["Individual_ID","Collection_ID","Source_Tissue","Country",
    "Location","Site","Latitude","Longitude","Date_C14_Labnr",
    "Date_C14_Uncal_BP","Date_C14_Uncal_BP_Err","Date_BC_AD_Median","Date_BC_AD_Start",
    "Date_BC_AD_Stop","Date_Type","No_of_Libraries","Data_Type","Genotype_Ploidy","Group_Name",
    "Genetic_Sex","Nr_autosomal_SNPs","Coverage_1240K","MT_Haplogroup","Y_Haplogroup",
    "Endogenous","UDG","Library_Built","Damage","Xcontam","Xcontam_stderr","mtContam",
    "mtContam_stderr","Primary_Contact","Publication_Status","Note","Keywords"
    ]

-- | A helper function to create semi-colon separated field values from lists
deparseFieldList :: (Csv.ToField a) => [a] -> Csv.Field
deparseFieldList xs = do
    Csv.toField $ intercalate ";" $ map (read . show . Csv.toField) xs

instance Csv.ToField [String] where
    toField = deparseFieldList

instance Csv.ToField [Int] where
    toField = deparseFieldList

instance Csv.ToField [JannoDataType] where
    toField = deparseFieldList

-- | A helper function to parse semi-colon separated field values into lists
parseFieldList :: (Csv.FromField a) => Csv.Field -> Csv.Parser [a]
parseFieldList x = do
    fieldStr <- Csv.parseField x
    let subStrings = Bchs.splitWith (==';') fieldStr
    mapM Csv.parseField subStrings

instance Csv.FromField [String] where
    parseField = parseFieldList

instance Csv.FromField [Int] where
    parseField = parseFieldList

instance Csv.FromField [JannoDataType] where
    parseField = parseFieldList

-- Janno file writing

writeJannoFile :: FilePath -> [JannoRow] -> IO ()
writeJannoFile path samples = do
    let jannoAsBytestring = Csv.encodeDefaultOrderedByNameWith encodingOptions samples
    let jannoAsBytestringwithNA = explicitNA jannoAsBytestring
    Bch.writeFile path jannoAsBytestringwithNA

encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    , Csv.encUseCrLf = False
    , Csv.encIncludeHeader = True
    , Csv.encQuoting = Csv.QuoteMinimal
}

-- | A function to load one janno file
readJannoFile :: Bool -- whether to print verbose output
              -> FilePath 
              -> IO [JannoRow]
readJannoFile verbose jannoPath = do
    when verbose $ do
        hPutStrLn stderr $ jannoPath ++ ":"
    jannoFile <- Bch.readFile jannoPath
    let jannoFileRows = Bch.lines jannoFile
    when verbose $ do
        hPutStrLn stderr $ show (length jannoFileRows - 1) ++ " samples in this file"
    -- tupel with row number and row bytestring
    let jannoFileRowsWithNumber = zip [1..(length jannoFileRows)] jannoFileRows
    -- filter out empty lines
        jannoFileRowsWithNumberFiltered = filter (\(x,y) -> y /= Bch.empty) jannoFileRowsWithNumber
    -- create header + individual line combination
        headerOnly = snd $ head jannoFileRowsWithNumberFiltered
        rowsOnly = tail jannoFileRowsWithNumberFiltered
        jannoFileRowsWithHeader = map (second (\x -> headerOnly <> "\n" <> x)) rowsOnly
    -- report missing or additional columns
    when verbose $ do
        let jannoColNames = map Bch.toStrict (Bch.split '\t' headerOnly)
            missing_columns = map Bchs.unpack $ jannoHeader \\ jannoColNames
            additional_columns = map Bchs.unpack $ jannoColNames \\ jannoHeader
        unless (null missing_columns) $ do
            hPutStrLn stderr $ "Missing standard columns: " ++ intercalate ", " missing_columns
        unless (null additional_columns) $ do
            hPutStrLn stderr $ "Additional standard columns: " ++ intercalate ", " additional_columns
    -- load janno by rows
    jannoRepresentation <- mapM (readJannoFileRow jannoPath) jannoFileRowsWithHeader
    -- error case management
    if not (null (lefts jannoRepresentation))
    then do
        mapM_ (hPutStrLn stderr . renderPoseidonException) $ take 5 $ lefts jannoRepresentation
        throwIO $ PoseidonJannoConsistencyException jannoPath "Broken lines"
    else do
        let consistentJanno = checkJannoConsistency jannoPath $ rights jannoRepresentation
        case consistentJanno of
            Left e -> do throwIO e
            Right x -> do
                -- putStrLn ""
                -- putStrLn $ show $ map jSourceTissue x
                -- putStrLn $ show $ map jLongitude x
                -- putStrLn $ show $ map jUDG x
                return x

-- | A function to load one row of a janno file
readJannoFileRow :: FilePath -> (Int, Bch.ByteString) -> IO (Either PoseidonException JannoRow)
readJannoFileRow jannoPath (lineNumber, row) = do
    case Csv.decodeByNameWith decodingOptions row of
        Left e -> do
            return $ Left $ PoseidonJannoRowException jannoPath lineNumber $ e
        Right (_, jannoRow :: V.Vector JannoRow) -> do
            case checkJannoRowConsistency jannoPath lineNumber $ V.head jannoRow of
                Left e -> do
                    return $ Left e
                Right (pS :: JannoRow) -> do
                    return $ Right pS

decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}

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

-- | A function to create empty janno rows for a set of individuals
createMinimalJanno :: [EigenstratIndEntry] -> [JannoRow]
createMinimalJanno = map createMinimalSample

-- | A function to create an empty janno row for an individual
createMinimalSample :: EigenstratIndEntry -> JannoRow
createMinimalSample (EigenstratIndEntry id sex pop) =
    JannoRow
        { jIndividualID      = id
        , jCollectionID      = Nothing
        , jSourceTissue      = Nothing
        , jCountry           = Nothing
        , jLocation          = Nothing
        , jSite              = Nothing
        , jLatitude          = Nothing
        , jLongitude         = Nothing
        , jDateC14Labnr      = Nothing
        , jDateC14UncalBP    = Nothing
        , jDateC14UncalBPErr = Nothing
        , jDateBCADMedian    = Nothing
        , jDateBCADStart     = Nothing
        , jDateBCADStop      = Nothing
        , jDateType          = Nothing
        , jNrLibraries       = Nothing
        , jDataType          = Nothing
        , jGenotypePloidy    = Nothing
        , jGroupName         = [pop]
        , jGeneticSex        = JannoSex sex
        , jNrAutosomalSNPs   = Nothing
        , jCoverage1240K     = Nothing
        , jMTHaplogroup      = Nothing
        , jYHaplogroup       = Nothing
        , jEndogenous        = Nothing
        , jUDG               = Nothing
        , jLibraryBuilt      = Nothing
        , jDamage            = Nothing
        , jNuclearContam     = Nothing
        , jNuclearContamErr  = Nothing
        , jMTContam          = Nothing
        , jMTContamErr       = Nothing
        , jPrimaryContact    = Nothing
        , jPublication       = Nothing
        , jComments          = Nothing
        , jKeywords          = Nothing
    }

-- Janno consistency checks

checkJannoConsistency :: FilePath -> [JannoRow] -> Either PoseidonException [JannoRow]
checkJannoConsistency jannoPath xs
    | not $ checkIndividualUnique xs = Left $ PoseidonJannoConsistencyException jannoPath
        "The Individual_IDs are not unique"
    | otherwise = Right xs

checkIndividualUnique :: [JannoRow] -> Bool
checkIndividualUnique x = length x == length (nub $ map jIndividualID x)

checkJannoRowConsistency :: FilePath -> Int -> JannoRow -> Either PoseidonException JannoRow
checkJannoRowConsistency jannoPath row x
    | not $ checkMandatoryStringNotEmpty x = Left $ PoseidonJannoRowException jannoPath row
          "The mandatory columns Individual_ID and Group_Name contain empty values"
    | not $ checkC14ColsConsistent x = Left $ PoseidonJannoRowException jannoPath row
          "The columns Date_C14_Labnr, Date_C14_Uncal_BP, Date_C14_Uncal_BP_Err and Date_Type are not consistent"
    | otherwise = Right x

checkMandatoryStringNotEmpty :: JannoRow -> Bool
checkMandatoryStringNotEmpty x =
    not (null $ jIndividualID x)
    && not (null (jGroupName x))
    && not (null $ head $ jGroupName x)

checkC14ColsConsistent :: JannoRow -> Bool
checkC14ColsConsistent x =
    let lLabnr = maybe 0 length $ jDateC14Labnr x
        lUncalBP = maybe 0 length $ jDateC14UncalBP x
        lUncalBPErr = maybe 0 length $ jDateC14UncalBPErr x
        shouldBeTypeC14 = lUncalBP > 0
        isTypeC14 = jDateType x == Just C14
    in
        (lLabnr == 0 || lUncalBP == 0 || lLabnr == lUncalBP)
        && (lLabnr == 0 || lUncalBPErr == 0 || lLabnr == lUncalBPErr)
        && lUncalBP == lUncalBPErr
        && ((not shouldBeTypeC14) || isTypeC14)
