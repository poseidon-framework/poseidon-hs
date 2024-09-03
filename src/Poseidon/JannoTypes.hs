{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.JannoTypes where

import qualified Data.Csv                             as Csv
import           SequenceFormats.Eigenstrat           (Sex (..))
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Aeson                           (FromJSON,
                                                       ToJSON, Value (..),
                                                       parseJSON,
                                                       toJSON, withText, toEncoding, genericToEncoding, defaultOptions)
import Data.ByteString as S
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Aeson.Types (Parser)
import           Country                              (Country, alphaTwoUpper,
                                                       decodeAlphaTwo)
import qualified Data.Text.Read as T
import           Network.URI                          (isURIReference)
import qualified Text.Regex.TDFA                      as Reg

-- a typeclass for types associated to a column name
class HasColName a where
    colname :: a -> String

-- a typeclass for types with smart constructors
class Makeable a where
    make :: MonadFail m => T.Text -> m a

-- helper functions
parseTypeCSV :: forall a m. (MonadFail m, Makeable a, HasColName a, Typeable a) => S.ByteString -> m a
parseTypeCSV x = case T.decodeUtf8' x of
        Left e  -> fail $ show e ++ " in column " ++ colname (undefined :: a)
        Right t -> make t

parseTypeJSON :: forall a. (Makeable a, HasColName a, Typeable a) => Value -> Parser a
parseTypeJSON = withText (colname (undefined :: a)) make

-- | A datatype for the Genetic_Sex .janno column
newtype JannoSex = JannoSex { sfSex :: Sex }
    deriving (Eq)

makeJannoSex :: MonadFail m => String -> m JannoSex
makeJannoSex x
    | x == "F"  = pure (JannoSex Female)
    | x == "M"  = pure (JannoSex Male)
    | x == "U"  = pure (JannoSex Unknown)
    | otherwise = fail $ "Genetic_Sex is set to " ++ show x ++ ". " ++
                         "That is not in the allowed set [F, M, U]."

instance Show JannoSex where
    show (JannoSex Female)  = "F"
    show (JannoSex Male)    = "M"
    show (JannoSex Unknown) = "U"
instance Ord JannoSex where
    compare (JannoSex Female) (JannoSex Male)    = GT
    compare (JannoSex Male) (JannoSex Female)    = LT
    compare (JannoSex Male) (JannoSex Unknown)   = GT
    compare (JannoSex Unknown) (JannoSex Male)   = LT
    compare (JannoSex Female) (JannoSex Unknown) = GT
    compare (JannoSex Unknown) (JannoSex Female) = LT
    compare _ _                                  = EQ
instance Csv.ToField JannoSex where   toField x    = Csv.toField $ show x
instance Csv.FromField JannoSex where parseField x = Csv.parseField x >>= makeJannoSex
instance ToJSON JannoSex where        toJSON x     = String $ T.pack $ show x
instance FromJSON JannoSex where      parseJSON    = withText "JannoSex" (makeJannoSex . T.unpack)

-- | A datatype for the Alternative_IDs .janno column
newtype JannoAlternativeID = JannoAlternativeID T.Text deriving (Eq)

instance HasColName JannoAlternativeID where    colname _ = "Alternative_IDs"
instance Makeable JannoAlternativeID where      make = pure . JannoAlternativeID
instance Show JannoAlternativeID where          show (JannoAlternativeID x) = T.unpack x
instance Csv.ToField JannoAlternativeID where   toField (JannoAlternativeID x) = Csv.toField x
instance Csv.FromField JannoAlternativeID where parseField = parseTypeCSV
instance ToJSON JannoAlternativeID where        toJSON (JannoAlternativeID x) = String x
instance FromJSON JannoAlternativeID where      parseJSON = parseTypeJSON

-- | A datatype for the Relation_To .janno column
newtype JannoRelationTo = JannoRelationTo T.Text deriving (Eq)

instance HasColName JannoRelationTo where    colname _ = "Relation_To"
instance Makeable JannoRelationTo where      make = pure . JannoRelationTo
instance Show JannoRelationTo where          show (JannoRelationTo x) = T.unpack x
instance Csv.ToField JannoRelationTo where   toField (JannoRelationTo x) = Csv.toField x
instance Csv.FromField JannoRelationTo where parseField = parseTypeCSV
instance ToJSON JannoRelationTo where        toJSON (JannoRelationTo x) = String x
instance FromJSON JannoRelationTo where      parseJSON = parseTypeJSON

-- | A datatype for the Relation_Degree .janno column
data JannoRelationDegree =
      Identical
    | First
    | Second
    | ThirdToFifth
    | SixthToTenth
    | Unrelated
    | OtherDegree
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance HasColName JannoRelationDegree where colname _ = "Relation_Degree"
instance Makeable JannoRelationDegree where
    make x
        | x == "identical"    = pure Identical
        | x == "first"        = pure First
        | x == "second"       = pure Second
        | x == "thirdToFifth" = pure ThirdToFifth
        | x == "sixthToTenth" = pure SixthToTenth
        | x == "unrelated"    = pure Unrelated -- this should be omitted in the documentation
                                               -- relations of type "unrelated" don't have to be
                                               -- listed explicitly
        | x == "other"        = pure OtherDegree
        | otherwise           = fail $ "Relation_Degree is set to " ++ show x ++ ". " ++
                                       "That is not in the allowed set [identical, first, second, thirdToFifth, sixthToTenth, other]."
instance Show JannoRelationDegree where
    show Identical    = "identical"
    show First        = "first"
    show Second       = "second"
    show ThirdToFifth = "thirdToFifth"
    show SixthToTenth = "sixthToTenth"
    show Unrelated    = "unrelated"
    show OtherDegree  = "other"
instance Csv.ToField JannoRelationDegree where   toField x = Csv.toField $ show x
instance Csv.FromField JannoRelationDegree where parseField = parseTypeCSV
instance ToJSON JannoRelationDegree where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoRelationDegree

-- | A datatype for the Relation_Type .janno column
newtype JannoRelationType = JannoRelationType T.Text deriving (Eq)

instance HasColName JannoRelationType where    colname _ = "Relation_Type"
instance Makeable JannoRelationType where      make = pure . JannoRelationType
instance Show JannoRelationType where          show (JannoRelationType x) = T.unpack x
instance Csv.ToField JannoRelationType where   toField (JannoRelationType x) = Csv.toField x
instance Csv.FromField JannoRelationType where parseField = parseTypeCSV
instance ToJSON JannoRelationType where        toJSON (JannoRelationType x) = String x
instance FromJSON JannoRelationType where      parseJSON = parseTypeJSON

-- | A datatype for the Relation_Note .janno column
newtype JannoRelationNote = JannoRelationNote T.Text deriving (Eq)

instance HasColName JannoRelationNote where    colname _ = "Relation_Note"
instance Makeable JannoRelationNote where      make = pure . JannoRelationNote
instance Show JannoRelationNote where          show (JannoRelationNote x) = T.unpack x
instance Csv.ToField JannoRelationNote where   toField (JannoRelationNote x) = Csv.toField x
instance Csv.FromField JannoRelationNote where parseField = parseTypeCSV
instance ToJSON JannoRelationNote where        toJSON (JannoRelationNote x) = String x
instance FromJSON JannoRelationNote where      parseJSON = parseTypeJSON

-- | A datatype for the Collection_ID .janno column
newtype JannoCollectionID = JannoCollectionID T.Text deriving (Eq)

instance HasColName JannoCollectionID where    colname _ = "Collection_ID"
instance Makeable JannoCollectionID where      make = pure . JannoCollectionID
instance Show JannoCollectionID where          show (JannoCollectionID x) = T.unpack x
instance Csv.ToField JannoCollectionID where   toField (JannoCollectionID x) = Csv.toField x
instance Csv.FromField JannoCollectionID where parseField = parseTypeCSV
instance ToJSON JannoCollectionID where        toJSON (JannoCollectionID x) = String x
instance FromJSON JannoCollectionID where      parseJSON = parseTypeJSON

-- | A datatype for the Collection_ID .janno column
newtype JannoCountry = JannoCountry T.Text deriving (Eq, Ord)

instance HasColName JannoCountry where    colname _ = "Country"
instance Makeable JannoCountry where      make = pure . JannoCountry
instance Show JannoCountry where          show (JannoCountry x) = T.unpack x
instance Csv.ToField JannoCountry where   toField (JannoCountry x) = Csv.toField x
instance Csv.FromField JannoCountry where parseField = parseTypeCSV
instance ToJSON JannoCountry where        toJSON (JannoCountry x) = String x
instance FromJSON JannoCountry where      parseJSON = parseTypeJSON

-- | A datatype for countries in ISO-alpha2 code format
newtype JannoCountryISO = JannoCountryISO Country deriving (Eq, Ord)

instance HasColName JannoCountryISO where colname _ = "Country_ISO"
instance Show JannoCountryISO where
    show (JannoCountryISO x) = T.unpack $ alphaTwoUpper x
instance Makeable JannoCountryISO where
    make x = case decodeAlphaTwo x of
        Nothing -> fail $
            "Country_ISO is set to " ++ show x ++ ". " ++
            "That is not a valid ISO-alpha2 code describing an existing country."
        Just c  -> return $ JannoCountryISO c
instance Csv.ToField JannoCountryISO where   toField x = Csv.toField $ show x
instance Csv.FromField JannoCountryISO where parseField = parseTypeCSV
instance ToJSON JannoCountryISO where        toJSON x  = String $ T.pack $ show x
instance FromJSON JannoCountryISO where      parseJSON = parseTypeJSON

-- | A datatype for the Location .janno column
newtype JannoLocation = JannoLocation T.Text deriving (Eq)

instance HasColName JannoLocation where    colname _ = "Collection_ID"
instance Makeable JannoLocation where      make = pure . JannoLocation
instance Show JannoLocation where          show (JannoLocation x) = T.unpack x
instance Csv.ToField JannoLocation where   toField (JannoLocation x) = Csv.toField x
instance Csv.FromField JannoLocation where parseField = parseTypeCSV
instance ToJSON JannoLocation where        toJSON (JannoLocation x) = String x
instance FromJSON JannoLocation where      parseJSON = parseTypeJSON

-- | A datatype for the Site .janno column
newtype JannoSite = JannoSite T.Text deriving (Eq, Ord)

instance HasColName JannoSite where    colname _ = "Site"
instance Makeable JannoSite where      make = pure . JannoSite
instance Show JannoSite where          show (JannoSite x) = T.unpack x
instance Csv.ToField JannoSite where   toField (JannoSite x) = Csv.toField x
instance Csv.FromField JannoSite where parseField = parseTypeCSV
instance ToJSON JannoSite where        toJSON (JannoSite x) = String x
instance FromJSON JannoSite where      parseJSON = parseTypeJSON

-- | A datatype for Latitudes
newtype Latitude = Latitude Double deriving (Eq, Ord, Generic)

instance HasColName Latitude where    colname _ = "Latitude"
instance Makeable Latitude where
    make x =
        case T.double x of
            Left e -> fail $ "Latitude can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= -90 && num <= 90
                then pure (Latitude num)
                else fail $ "Latitude " ++ show x ++ " not between -90 and 90"
            Right (_, rest) -> fail $ "Latitude can not be converted to Double, because of a trailing " ++ show rest
instance Show Latitude where          show (Latitude x) = show x
instance Csv.ToField Latitude where   toField (Latitude x) = Csv.toField x
instance Csv.FromField Latitude where parseField = parseTypeCSV
instance ToJSON Latitude where        toEncoding = genericToEncoding defaultOptions
instance FromJSON Latitude

-- | A datatype for Longitudes
newtype Longitude = Longitude Double deriving (Eq, Ord, Generic)

instance HasColName Longitude where    colname _ = "Longitude"
instance Makeable Longitude where
    make x =
        case T.double x of
            Left e -> fail $ "Longitude can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= -180 && num <= 180
                then pure (Longitude num)
                else fail $ "Longitude " ++ show x ++ " not between -180 and 180"
            Right (_, rest) -> fail $ "Longitude can not be converted to Double, because of a trailing " ++ show rest
instance Show Longitude where          show (Longitude x) = show x
instance Csv.ToField Longitude where   toField (Longitude x) = Csv.toField x
instance Csv.FromField Longitude where parseField = parseTypeCSV
instance ToJSON Longitude where        toEncoding = genericToEncoding defaultOptions
instance FromJSON Longitude

-- | A datatype for the Date_Type .janno column
data JannoDateType =
      C14
    | Contextual
    | Modern
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance HasColName JannoDateType where    colname _ = "Date_Type"
instance Makeable JannoDateType where
    make x
        | x == "C14"        = pure C14
        | x == "contextual" = pure Contextual
        | x == "modern"     = pure Modern
        | otherwise         = fail $ "Date_Type is set to " ++ show x ++ ". " ++
                                     "That is not in the allowed set [C14, contextual, modern]."
instance Show JannoDateType where
    show C14        = "C14"
    show Contextual = "contextual"
    show Modern     = "modern"
instance Csv.ToField JannoDateType where   toField x = Csv.toField $ show x
instance Csv.FromField JannoDateType where parseField = parseTypeCSV
instance ToJSON JannoDateType where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoDateType

-- | A datatype for the Date_C14_Labnr .janno column
newtype JannoDateC14Labnr = JannoDateC14Labnr T.Text deriving (Eq)

instance HasColName JannoDateC14Labnr where    colname _ = "Date_C14_Labnr"
instance Makeable JannoDateC14Labnr where      make = pure . JannoDateC14Labnr
instance Show JannoDateC14Labnr where          show (JannoDateC14Labnr x) = T.unpack x
instance Csv.ToField JannoDateC14Labnr where   toField (JannoDateC14Labnr x) = Csv.toField x
instance Csv.FromField JannoDateC14Labnr where parseField = parseTypeCSV
instance ToJSON JannoDateC14Labnr where        toJSON (JannoDateC14Labnr x) = String x
instance FromJSON JannoDateC14Labnr where      parseJSON = parseTypeJSON

-- | A datatype for the Date_C14_Uncal_BP .janno column
newtype JannoDateC14UncalBP = JannoDateC14UncalBP Int deriving (Eq, Ord, Generic)

instance HasColName JannoDateC14UncalBP where    colname _ = "Date_C14_Uncal_BP"
instance Makeable JannoDateC14UncalBP where
    make x =
        case T.decimal x of
            Left e -> fail $ "Date_C14_Uncal_BP can not be converted to Int because " ++ e
            Right (num, "") -> pure $ JannoDateC14UncalBP num
            Right (_, rest) -> fail $ "Date_C14_Uncal_BP can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateC14UncalBP where          show (JannoDateC14UncalBP x) = show x
instance Csv.ToField JannoDateC14UncalBP where   toField (JannoDateC14UncalBP x) = Csv.toField x
instance Csv.FromField JannoDateC14UncalBP where parseField = parseTypeCSV
instance ToJSON JannoDateC14UncalBP where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoDateC14UncalBP

-- | A datatype for the Date_C14_Uncal_BP_Err .janno column
newtype JannoDateC14UncalBPErr = JannoDateC14UncalBPErr Int deriving (Eq, Ord, Generic)

instance HasColName JannoDateC14UncalBPErr where    colname _ = "Date_C14_Uncal_BP_Err"
instance Makeable JannoDateC14UncalBPErr where
    make x =
        case T.decimal x of
            Left e -> fail $ "Date_C14_Uncal_BP_Err can not be converted to Int because " ++ e
            Right (num, "") -> pure $ JannoDateC14UncalBPErr num
            Right (_, rest) -> fail $ "Date_C14_Uncal_BP_Err can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoDateC14UncalBPErr where          show (JannoDateC14UncalBPErr x) = show x
instance Csv.ToField JannoDateC14UncalBPErr where   toField (JannoDateC14UncalBPErr x) = Csv.toField x
instance Csv.FromField JannoDateC14UncalBPErr where parseField = parseTypeCSV
instance ToJSON JannoDateC14UncalBPErr where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoDateC14UncalBPErr

-- | A datatype for the Date_BC_AD_Start .janno column
newtype DateBCADStart = DateBCADStart Int deriving (Eq, Ord, Generic)

instance HasColName DateBCADStart where    colname _ = "Date_BC_AD_Start"
instance Makeable DateBCADStart where
    make x =
        let curYear = 2024 -- the current year
        in case T.signed T.decimal x of
            Left e -> fail $ "Date_BC_AD_Start can not be converted to Int because " ++ e
            Right (num, "") ->
                if num >= curYear
                then fail $ "Date_BC_AD_Start " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
                else pure $ DateBCADStart num
            Right (_, rest) -> fail $ "Date_BC_AD_Start can not be converted to Int, because of a trailing " ++ show rest
instance Show DateBCADStart where          show (DateBCADStart x) = show x
instance Csv.ToField DateBCADStart where   toField (DateBCADStart x) = Csv.toField x
instance Csv.FromField DateBCADStart where parseField = parseTypeCSV
instance ToJSON DateBCADStart where        toEncoding = genericToEncoding defaultOptions
instance FromJSON DateBCADStart

-- | A datatype for the Date_BC_AD_Median .janno column
newtype DateBCADMedian = DateBCADMedian Int deriving (Eq, Ord, Generic)

instance HasColName DateBCADMedian where    colname _ = "Date_BC_AD_Median"
instance Makeable DateBCADMedian where
    make x =
        let curYear = 2024 -- the current year
        in case T.signed T.decimal x of
            Left e -> fail $ "Date_BC_AD_Median can not be converted to Int because " ++ e
            Right (num, "") ->
                if num >= curYear
                then fail $ "Date_BC_AD_Median " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
                else pure $ DateBCADMedian num
            Right (_, rest) -> fail $ "Date_BC_AD_Median can not be converted to Int, because of a trailing " ++ show rest
instance Show DateBCADMedian where          show (DateBCADMedian x) = show x
instance Csv.ToField DateBCADMedian where   toField (DateBCADMedian x) = Csv.toField x
instance Csv.FromField DateBCADMedian where parseField = parseTypeCSV
instance ToJSON DateBCADMedian where        toEncoding = genericToEncoding defaultOptions
instance FromJSON DateBCADMedian

-- | A datatype for the Date_BC_AD_Stop .janno column
newtype DateBCADStop = DateBCADStop Int deriving (Eq, Ord, Generic)

instance HasColName DateBCADStop where    colname _ = "Date_BC_AD_Stop"
instance Makeable DateBCADStop where
    make x =
        let curYear = 2024 -- the current year
        in case T.signed T.decimal x of
            Left e -> fail $ "Date_BC_AD_Stop can not be converted to Int because " ++ e
            Right (num, "") ->
                if num >= curYear
                then fail $ "Date_BC_AD_Stop " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
                else pure $ DateBCADStop num
            Right (_, rest) -> fail $ "Date_BC_AD_Stop can not be converted to Int, because of a trailing " ++ show rest
instance Show DateBCADStop where          show (DateBCADStop x) = show x
instance Csv.ToField DateBCADStop where   toField (DateBCADStop x) = Csv.toField x
instance Csv.FromField DateBCADStop where parseField = parseTypeCSV
instance ToJSON DateBCADStop where        toEncoding = genericToEncoding defaultOptions
instance FromJSON DateBCADStop

-- | A datatype for the Date_Note .janno column
newtype JannoDateNote = JannoDateNote T.Text deriving (Eq, Ord)

instance HasColName JannoDateNote where    colname _ = "Date_Note"
instance Makeable JannoDateNote where      make = pure . JannoDateNote
instance Show JannoDateNote where          show (JannoDateNote x) = T.unpack x
instance Csv.ToField JannoDateNote where   toField (JannoDateNote x) = Csv.toField x
instance Csv.FromField JannoDateNote where parseField = parseTypeCSV
instance ToJSON JannoDateNote where        toJSON (JannoDateNote x) = String x
instance FromJSON JannoDateNote where      parseJSON = parseTypeJSON

-- | A datatype for the MT_Haplogroup .janno column
newtype JannoMTHaplogroup = JannoMTHaplogroup T.Text deriving (Eq, Ord)

instance HasColName JannoMTHaplogroup where    colname _ = "MT_Haplogroup"
instance Makeable JannoMTHaplogroup where      make = pure . JannoMTHaplogroup
instance Show JannoMTHaplogroup where          show (JannoMTHaplogroup x) = T.unpack x
instance Csv.ToField JannoMTHaplogroup where   toField (JannoMTHaplogroup x) = Csv.toField x
instance Csv.FromField JannoMTHaplogroup where parseField = parseTypeCSV
instance ToJSON JannoMTHaplogroup where        toJSON (JannoMTHaplogroup x) = String x
instance FromJSON JannoMTHaplogroup where      parseJSON = parseTypeJSON

-- | A datatype for the Y_Haplogroup .janno column
newtype JannoYHaplogroup = JannoYHaplogroup T.Text deriving (Eq, Ord)

instance HasColName JannoYHaplogroup where    colname _ = "Y_Haplogroup"
instance Makeable JannoYHaplogroup where      make = pure . JannoYHaplogroup
instance Show JannoYHaplogroup where          show (JannoYHaplogroup x) = T.unpack x
instance Csv.ToField JannoYHaplogroup where   toField (JannoYHaplogroup x) = Csv.toField x
instance Csv.FromField JannoYHaplogroup where parseField = parseTypeCSV
instance ToJSON JannoYHaplogroup where        toJSON (JannoYHaplogroup x) = String x
instance FromJSON JannoYHaplogroup where      parseJSON = parseTypeJSON

-- | A datatype for the Source_Tissue .janno column
newtype JannoSourceTissue = JannoSourceTissue T.Text deriving (Eq)

instance HasColName JannoSourceTissue where    colname _ = "Source_Tissue"
instance Makeable JannoSourceTissue where      make = pure . JannoSourceTissue
instance Show JannoSourceTissue where          show (JannoSourceTissue x) = T.unpack x
instance Csv.ToField JannoSourceTissue where   toField (JannoSourceTissue x) = Csv.toField x
instance Csv.FromField JannoSourceTissue where parseField = parseTypeCSV
instance ToJSON JannoSourceTissue where        toJSON (JannoSourceTissue x) = String x
instance FromJSON JannoSourceTissue where      parseJSON = parseTypeJSON

-- | A datatype for the Nr_Libraries .janno column
newtype JannoNrLibraries = JannoNrLibraries Int deriving (Eq, Ord, Generic)

instance HasColName JannoNrLibraries where    colname _ = "Nr_Libraries"
instance Makeable JannoNrLibraries where
    make x =
        case T.signed T.decimal x of
            Left e -> fail $ "Nr_Libraries can not be converted to Int because " ++ e
            Right (num, "") ->
                if num < 1
                then fail $ "Nr_Libraries " ++ show x ++ " lower than 1, which is impossible."
                else pure $ JannoNrLibraries num
            Right (_, rest) -> fail $ "Nr_Libraries can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoNrLibraries where          show (JannoNrLibraries x) = show x
instance Csv.ToField JannoNrLibraries where   toField (JannoNrLibraries x) = Csv.toField x
instance Csv.FromField JannoNrLibraries where parseField = parseTypeCSV
instance ToJSON JannoNrLibraries where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoNrLibraries

-- | A datatype for the Library_Names .janno column
newtype JannoLibraryName = JannoLibraryName T.Text deriving (Eq)

instance HasColName JannoLibraryName where    colname _ = "Library_Names"
instance Makeable JannoLibraryName where      make = pure . JannoLibraryName
instance Show JannoLibraryName where          show (JannoLibraryName x) = T.unpack x
instance Csv.ToField JannoLibraryName where   toField (JannoLibraryName x) = Csv.toField x
instance Csv.FromField JannoLibraryName where parseField = parseTypeCSV
instance ToJSON JannoLibraryName where        toJSON (JannoLibraryName x) = String x
instance FromJSON JannoLibraryName where      parseJSON = parseTypeJSON

-- | A datatype for the Capture_Type .janno column
data JannoCaptureType =
      Shotgun
    | A1240K
    | ArborComplete
    | ArborPrimePlus
    | ArborAncestralPlus
    | TwistAncientDNA
    | OtherCapture
    | ReferenceGenome
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance HasColName JannoCaptureType where colname _ = "Capture_Type"
instance Makeable JannoCaptureType where
    make x
        | x == "Shotgun"            = pure Shotgun
        | x == "1240K"              = pure A1240K
        | x == "ArborComplete"      = pure ArborComplete
        | x == "ArborPrimePlus"     = pure ArborPrimePlus
        | x == "ArborAncestralPlus" = pure ArborAncestralPlus
        | x == "TwistAncientDNA"    = pure TwistAncientDNA
        | x == "OtherCapture"       = pure OtherCapture
        | x == "ReferenceGenome"    = pure ReferenceGenome
        | otherwise = fail $ "Capture_Type is set to " ++ show x ++ ". " ++
                             "That is not in the allowed set [Shotgun, 1240K, ArborComplete, ArborPrimePlus, ArborAncestralPlus, TwistAncientDNA, OtherCapture, ReferenceGenome]."
instance Show JannoCaptureType where
    show Shotgun            = "Shotgun"
    show A1240K             = "1240K"
    show ArborComplete      = "ArborComplete"
    show ArborPrimePlus     = "ArborPrimePlus"
    show ArborAncestralPlus = "ArborAncestralPlus"
    show TwistAncientDNA    = "TwistAncientDNA"
    show OtherCapture       = "OtherCapture"
    show ReferenceGenome    = "ReferenceGenome"
instance Csv.ToField JannoCaptureType where   toField x = Csv.toField $ show x
instance Csv.FromField JannoCaptureType where parseField = parseTypeCSV
instance ToJSON JannoCaptureType where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoCaptureType

-- | A datatype for the UDG .janno column
data JannoUDG =
      Minus
    | Half
    | Plus
    | Mixed
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance HasColName JannoUDG where colname _ = "UDG"
instance Makeable JannoUDG where
    make x
        | x == "minus" = pure Minus
        | x == "half"  = pure Half
        | x == "plus"  = pure Plus
        | x == "mixed" = pure Mixed
        | otherwise    = fail $ "UDG is set to " ++ show x ++ ". " ++
                                "That is not in the allowed set [minus, half, plus, mixed]."
instance Show JannoUDG where
    show Minus = "minus"
    show Half  = "half"
    show Plus  = "plus"
    show Mixed = "mixed"
instance Csv.ToField JannoUDG where   toField x = Csv.toField $ show x
instance Csv.FromField JannoUDG where parseField = parseTypeCSV
instance ToJSON JannoUDG where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoUDG

-- | A datatype for the Library_Built .janno column
data JannoLibraryBuilt =
      DS
    | SS
    | MixedSSDS
    | Other -- the "other" option is deprecated and should be removed at some point
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance HasColName JannoLibraryBuilt where colname _ = "Library_Built"
instance Makeable JannoLibraryBuilt where
    make x
        | x == "ds"    = pure DS
        | x == "ss"    = pure SS
        | x == "mixed" = pure MixedSSDS
        | x == "other" = pure Other
        | otherwise    = fail $ "Library_Built is set to " ++ show x ++ ". " ++
                                "That is not in the allowed set [ds, ss, mixed]."
instance Show JannoLibraryBuilt where
    show DS        = "ds"
    show SS        = "ss"
    show MixedSSDS = "mixed"
    show Other     = "other"
instance Csv.ToField JannoLibraryBuilt where   toField x = Csv.toField $ show x
instance Csv.FromField JannoLibraryBuilt where parseField = parseTypeCSV
instance ToJSON JannoLibraryBuilt where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoLibraryBuilt

-- | A datatype for the Genotype_Ploidy .janno column
data JannoGenotypePloidy =
      Diploid
    | Haploid
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance HasColName JannoGenotypePloidy where colname _ = "Genotype_Ploidy"
instance Makeable JannoGenotypePloidy where
    make x
        | x == "diploid" = pure Diploid
        | x == "haploid" = pure Haploid
        | otherwise      = fail $ "Genotype_Ploidy is set to " ++ show x ++ ". " ++
                                  "That is not in the allowed set [diploid, haploid]."
instance Show JannoGenotypePloidy where
    show Diploid = "diploid"
    show Haploid = "haploid"
instance Csv.ToField JannoGenotypePloidy where   toField x = Csv.toField $ show x
instance Csv.FromField JannoGenotypePloidy where parseField = parseTypeCSV
instance ToJSON JannoGenotypePloidy where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoGenotypePloidy

-- | A datatype for the Genotype_Ploidy .janno column
newtype JannoDataPreparationPipelineURL = JannoDataPreparationPipelineURL T.Text deriving (Eq, Ord, Generic)

instance HasColName JannoDataPreparationPipelineURL where colname _ = "Data_Preparation_Pipeline_URL"
instance Makeable JannoDataPreparationPipelineURL where
    make x
        | isURIReference (T.unpack x) = pure $ JannoDataPreparationPipelineURL x
        | otherwise                   = fail $ "Data_Preparation_Pipeline_URL " ++ show x ++ " is not a well structured URI."
instance Show JannoDataPreparationPipelineURL where          show (JannoDataPreparationPipelineURL x) = T.unpack x
instance Csv.ToField JannoDataPreparationPipelineURL where   toField (JannoDataPreparationPipelineURL x) = Csv.toField x
instance Csv.FromField JannoDataPreparationPipelineURL where parseField = parseTypeCSV
instance ToJSON JannoDataPreparationPipelineURL where        toJSON (JannoDataPreparationPipelineURL x) = String x
instance FromJSON JannoDataPreparationPipelineURL where      parseJSON = parseTypeJSON

-- | A datatype for the Endogenous .janno column
newtype JannoEndogenous = JannoEndogenous Double deriving (Eq, Ord, Generic)

instance HasColName JannoEndogenous where colname _ = "Endogenous"
instance Makeable JannoEndogenous where
    make x =
        case T.double x of
            Left e -> fail $ "Endogenous can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= 0 && num <= 100
                then pure (JannoEndogenous num)
                else fail $ "Endogenous " ++ show x ++ " not between 0 and 100."
            Right (_, rest) -> fail $ "Endogenous can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoEndogenous where          show (JannoEndogenous x) = show x
instance Csv.ToField JannoEndogenous where   toField (JannoEndogenous x) = Csv.toField x
instance Csv.FromField JannoEndogenous where parseField = parseTypeCSV
instance ToJSON JannoEndogenous where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoEndogenous

-- | A datatype for the Nr_SNPs .janno column
newtype JannoNrSNPs = JannoNrSNPs Int deriving (Eq, Ord, Generic)

instance HasColName JannoNrSNPs where    colname _ = "Nr_SNPs"
instance Makeable JannoNrSNPs where
    make x =
        case T.signed T.decimal x of
            Left e -> fail $ "Nr_SNPs can not be converted to Int because " ++ e
            Right (num, "") ->
                if num < 1
                then fail $ "Nr_SNPs " ++ show x ++ " lower than 1, which is not meaningful."
                else pure $ JannoNrSNPs num
            Right (_, rest) -> fail $ "Nr_SNPs can not be converted to Int, because of a trailing " ++ show rest
instance Show JannoNrSNPs where          show (JannoNrSNPs x) = show x
instance Csv.ToField JannoNrSNPs where   toField (JannoNrSNPs x) = Csv.toField x
instance Csv.FromField JannoNrSNPs where parseField = parseTypeCSV
instance ToJSON JannoNrSNPs where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoNrSNPs

-- | A datatype for the Coverage_on_Target_SNPs .janno column
newtype JannoCoverageOnTargets = JannoCoverageOnTargets Double deriving (Eq, Ord, Generic)

instance HasColName JannoCoverageOnTargets where colname _ = "Coverage_on_Target_SNPs"
instance Makeable JannoCoverageOnTargets where
    make x =
        case T.double x of
            Left e -> fail $ "Coverage_on_Target_SNPs can not be converted to Double because " ++ e
            Right (num, "") ->  pure (JannoCoverageOnTargets num)
            Right (_, rest) -> fail $ "Coverage_on_Target_SNPs can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoCoverageOnTargets where          show (JannoCoverageOnTargets x) = show x
instance Csv.ToField JannoCoverageOnTargets where   toField (JannoCoverageOnTargets x) = Csv.toField x
instance Csv.FromField JannoCoverageOnTargets where parseField = parseTypeCSV
instance ToJSON JannoCoverageOnTargets where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoCoverageOnTargets

-- | A datatype for the Damage .janno column
newtype JannoDamage = JannoDamage Double deriving (Eq, Ord, Generic)

instance HasColName JannoDamage where colname _ = "Damage"
instance Makeable JannoDamage where
    make x =
        case T.double x of
            Left e -> fail $ "Damage can not be converted to Double because " ++ e
            Right (num, "") ->
                if num >= 0 && num <= 100
                then pure (JannoDamage num)
                else fail $ "Damage " ++ show x ++ " not between 0 and 100."
            Right (_, rest) -> fail $ "Damage can not be converted to Double, because of a trailing " ++ show rest
instance Show JannoDamage where          show (JannoDamage x) = show x
instance Csv.ToField JannoDamage where   toField (JannoDamage x) = Csv.toField x
instance Csv.FromField JannoDamage where parseField = parseTypeCSV
instance ToJSON JannoDamage where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoDamage

-- | A datatype for the Contamination .janno column
newtype JannoContamination = JannoContamination T.Text deriving (Eq)

instance HasColName JannoContamination where    colname _ = "Contamination"
instance Makeable JannoContamination where      make = pure . JannoContamination
instance Show JannoContamination where          show (JannoContamination x) = T.unpack x
instance Csv.ToField JannoContamination where   toField (JannoContamination x) = Csv.toField x
instance Csv.FromField JannoContamination where parseField = parseTypeCSV
instance ToJSON JannoContamination where        toJSON (JannoContamination x) = String x
instance FromJSON JannoContamination where      parseJSON = parseTypeJSON

-- | A datatype for the Contamination_Err .janno column
newtype JannoContaminationErr = JannoContaminationErr T.Text deriving (Eq)

instance HasColName JannoContaminationErr where    colname _ = "Contamination_Err"
instance Makeable JannoContaminationErr where      make = pure . JannoContaminationErr
instance Show JannoContaminationErr where          show (JannoContaminationErr x) = T.unpack x
instance Csv.ToField JannoContaminationErr where   toField (JannoContaminationErr x) = Csv.toField x
instance Csv.FromField JannoContaminationErr where parseField = parseTypeCSV
instance ToJSON JannoContaminationErr where        toJSON (JannoContaminationErr x) = String x
instance FromJSON JannoContaminationErr where      parseJSON = parseTypeJSON

-- | A datatype for the Contamination_Meas .janno column
newtype JannoContaminationMeas = JannoContaminationMeas T.Text deriving (Eq)

instance HasColName JannoContaminationMeas where    colname _ = "Contamination_Meas"
instance Makeable JannoContaminationMeas where      make = pure . JannoContaminationMeas
instance Show JannoContaminationMeas where          show (JannoContaminationMeas x) = T.unpack x
instance Csv.ToField JannoContaminationMeas where   toField (JannoContaminationMeas x) = Csv.toField x
instance Csv.FromField JannoContaminationMeas where parseField = parseTypeCSV
instance ToJSON JannoContaminationMeas where        toJSON (JannoContaminationMeas x) = String x
instance FromJSON JannoContaminationMeas where      parseJSON = parseTypeJSON

-- | A datatype for the Contamination_Note .janno column
newtype JannoContaminationNote = JannoContaminationNote T.Text deriving (Eq)

instance HasColName JannoContaminationNote where    colname _ = "Contamination_Note"
instance Makeable JannoContaminationNote where      make = pure . JannoContaminationNote
instance Show JannoContaminationNote where          show (JannoContaminationNote x) = T.unpack x
instance Csv.ToField JannoContaminationNote where   toField (JannoContaminationNote x) = Csv.toField x
instance Csv.FromField JannoContaminationNote where parseField = parseTypeCSV
instance ToJSON JannoContaminationNote where        toJSON (JannoContaminationNote x) = String x
instance FromJSON JannoContaminationNote where      parseJSON = parseTypeJSON

-- | A datatype for the Genetic_Source_Accession_IDs .janno column
data JannoGeneticSourceAccessionID =
      INSDCProject T.Text
    | INSDCStudy T.Text
    | INSDCBioSample T.Text
    | INSDCSample T.Text
    | INSDCExperiment T.Text
    | INSDCRun T.Text
    | INSDCAnalysis T.Text
    | OtherID T.Text
    deriving (Eq, Ord, Generic)

instance HasColName JannoGeneticSourceAccessionID where    colname _ = "Genetic_Source_Accession_IDs"
instance Makeable JannoGeneticSourceAccessionID where
    make x
        | (T.unpack x) Reg.=~ ("PRJ[EDN][A-Z][0-9]+"  :: String) = pure $ INSDCProject x
        | (T.unpack x) Reg.=~ ("[EDS]RP[0-9]{6,}"     :: String) = pure $ INSDCStudy x
        | (T.unpack x) Reg.=~ ("SAM[EDN][A-Z]?[0-9]+" :: String) = pure $ INSDCBioSample x
        | (T.unpack x) Reg.=~ ("[EDS]RS[0-9]{6,}"     :: String) = pure $ INSDCSample x
        | (T.unpack x) Reg.=~ ("[EDS]RX[0-9]{6,}"     :: String) = pure $ INSDCExperiment x
        | (T.unpack x) Reg.=~ ("[EDS]RR[0-9]{6,}"     :: String) = pure $ INSDCRun x
        | (T.unpack x) Reg.=~ ("[EDS]RZ[0-9]{6,}"     :: String) = pure $ INSDCAnalysis x
        | otherwise                                   = pure $ OtherID x
instance Show JannoGeneticSourceAccessionID where
    show (INSDCProject x)    = T.unpack x
    show (INSDCStudy x)      = T.unpack x
    show (INSDCBioSample x)  = T.unpack x
    show (INSDCSample x)     = T.unpack x
    show (INSDCExperiment x) = T.unpack x
    show (INSDCRun x)        = T.unpack x
    show (INSDCAnalysis x)   = T.unpack x
    show (OtherID x)         = T.unpack x
instance Csv.ToField JannoGeneticSourceAccessionID where   toField x  = Csv.toField $ show x
instance Csv.FromField JannoGeneticSourceAccessionID where parseField = parseTypeCSV
instance ToJSON JannoGeneticSourceAccessionID where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoGeneticSourceAccessionID

-- | A datatype for the Primary_Contact .janno column
newtype JannoPrimaryContact = JannoPrimaryContact T.Text deriving (Eq)

instance HasColName JannoPrimaryContact where    colname _ = "Primary_Contact"
instance Makeable JannoPrimaryContact where      make = pure . JannoPrimaryContact
instance Show JannoPrimaryContact where          show (JannoPrimaryContact x) = T.unpack x
instance Csv.ToField JannoPrimaryContact where   toField (JannoPrimaryContact x) = Csv.toField x
instance Csv.FromField JannoPrimaryContact where parseField = parseTypeCSV
instance ToJSON JannoPrimaryContact where        toJSON (JannoPrimaryContact x) = String x
instance FromJSON JannoPrimaryContact where      parseJSON = parseTypeJSON

-- | A datatype for the Publication .janno column
newtype JannoPublication = JannoPublication T.Text deriving (Eq, Ord)

instance HasColName JannoPublication where    colname _ = "Publication"
instance Makeable JannoPublication where      make = pure . JannoPublication
instance Show JannoPublication where          show (JannoPublication x) = T.unpack x
instance Csv.ToField JannoPublication where   toField (JannoPublication x) = Csv.toField x
instance Csv.FromField JannoPublication where parseField = parseTypeCSV
instance ToJSON JannoPublication where        toJSON (JannoPublication x) = String x
instance FromJSON JannoPublication where      parseJSON = parseTypeJSON

-- | A datatype for the Note .janno column
newtype JannoComment = JannoComment T.Text deriving (Eq)

instance HasColName JannoComment where    colname _ = "Note"
instance Makeable JannoComment where      make = pure . JannoComment
instance Show JannoComment where          show (JannoComment x) = T.unpack x
instance Csv.ToField JannoComment where   toField (JannoComment x) = Csv.toField x
instance Csv.FromField JannoComment where parseField = parseTypeCSV
instance ToJSON JannoComment where        toJSON (JannoComment x) = String x
instance FromJSON JannoComment where      parseJSON = parseTypeJSON

-- | A datatype for the Keywords .janno column
newtype JannoKeyword = JannoKeyword T.Text deriving (Eq)

instance HasColName JannoKeyword where    colname _ = "Keywords"
instance Makeable JannoKeyword where      make = pure . JannoKeyword
instance Show JannoKeyword where          show (JannoKeyword x) = T.unpack x
instance Csv.ToField JannoKeyword where   toField (JannoKeyword x) = Csv.toField x
instance Csv.FromField JannoKeyword where parseField = parseTypeCSV
instance ToJSON JannoKeyword where        toJSON (JannoKeyword x) = String x
instance FromJSON JannoKeyword where      parseJSON = parseTypeJSON