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

instance HasColName JannoSite where    colname _ = "Country"
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
