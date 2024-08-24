{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings   #-}

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

-- a typeclass for types with smart constructors
class Makeable a where
    make :: MonadFail m => T.Text -> m a

-- helper functions
parseType :: (MonadFail m, Makeable a) => String -> S.ByteString -> m a
parseType colName x = case T.decodeUtf8' x of
        Left e  -> fail $ show e ++ " in column " ++ colName
        Right t -> make t

-- | A datatype for the Genetic_Sex .janno column
newtype JannoSex = JannoSex { sfSex :: Sex }
    deriving (Eq)

makeJannoSex :: MonadFail m => String -> m JannoSex
makeJannoSex x
    | x == "F"  = pure (JannoSex Female)
    | x == "M"  = pure (JannoSex Male)
    | x == "U"  = pure (JannoSex Unknown)
    | otherwise = fail $ "Genetic_Sex is set to " ++ show x ++ "." ++
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

instance Makeable JannoAlternativeID where      make = pure . JannoAlternativeID
instance Show JannoAlternativeID where          show (JannoAlternativeID x) = T.unpack x
instance Csv.ToField JannoAlternativeID where   toField (JannoAlternativeID x) = Csv.toField x
instance Csv.FromField JannoAlternativeID where parseField = parseType "Alternative_IDs"
instance ToJSON JannoAlternativeID where        toJSON (JannoAlternativeID x) = String x
instance FromJSON JannoAlternativeID where      parseJSON = withText "Alternative_IDs" make

-- | A datatype for the Relation_To .janno column
newtype JannoRelationTo = JannoRelationTo T.Text deriving (Eq)

instance Makeable JannoRelationTo where      make = pure . JannoRelationTo
instance Show JannoRelationTo where          show (JannoRelationTo x) = T.unpack x
instance Csv.ToField JannoRelationTo where   toField (JannoRelationTo x) = Csv.toField x
instance Csv.FromField JannoRelationTo where parseField = parseType "Relation_To"
instance ToJSON JannoRelationTo where        toJSON (JannoRelationTo x) = String x
instance FromJSON JannoRelationTo where      parseJSON = withText "Relation_To" make

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
        | otherwise           = fail $ "Relation_Degree is set to " ++ show x ++ "." ++
                                       "That is not in the allowed set [identical, first, second, thirdToFifth, sixthToTenth, other]"
instance Show JannoRelationDegree where
    show Identical    = "identical"
    show First        = "first"
    show Second       = "second"
    show ThirdToFifth = "thirdToFifth"
    show SixthToTenth = "sixthToTenth"
    show Unrelated    = "unrelated"
    show OtherDegree  = "other"
instance Csv.ToField JannoRelationDegree where   toField x = Csv.toField $ show x
instance Csv.FromField JannoRelationDegree where parseField = parseType "Relation_Degree"
instance ToJSON JannoRelationDegree where        toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoRelationDegree