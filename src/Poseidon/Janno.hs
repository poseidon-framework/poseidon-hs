{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.Janno (
    JannoRow(..),
    JannoSex (..),
    JannoList (..),
    Sex (..),
    Latitude (..),
    Longitude (..),
    JannoDateType (..),
    JannoCaptureType (..),
    JannoGenotypePloidy (..),
    Percent (..),
    JannoUDG (..),
    JURI (..),
    JannoLibraryBuilt (..),
    writeJannoFile,
    readJannoFile,
    createMinimalJanno,
    jannoHeaderString
) where

import           Poseidon.Utils             (PoseidonException (..),
                                             renderPoseidonException)


import           Control.Applicative        (empty)
import           Control.Exception          (throwIO)
import           Control.Monad              (unless, when)
import           Data.Aeson                 (FromJSON, Options (..), ToJSON, Value(..),
                                             defaultOptions, genericToEncoding, toJSON, parseJSON,
                                             toEncoding)
import           Data.Bifunctor             (second)
import qualified Data.ByteString.Char8      as Bchs
import qualified Data.ByteString.Lazy.Char8 as Bch
import           Data.Char                  (ord)
import qualified Data.Csv                   as Csv
import           Data.Either                (lefts, rights)
import qualified Data.HashMap.Lazy          as HM
import           Data.List                  (intercalate, nub, (\\), elemIndex)
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Text                  (pack, unpack, replace)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)
import           Network.URI                (isURI)
import           Options.Applicative.Help.Levenshtein (editDistance)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..), Sex (..))
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
        | x == "F"             = pure (JannoSex Female)
        | x == "M"             = pure (JannoSex Male)
        | x == "U"             = pure (JannoSex Unknown)
        | otherwise            = fail $ "Sex " ++ show x ++ " not in [F, M, U]"
 
instance Csv.ToField JannoSex where
    toField (JannoSex Female)  = "F"
    toField (JannoSex Male)    = "M"
    toField (JannoSex Unknown) = "U"

instance FromJSON JannoSex where
    parseJSON (String "F")     = pure (JannoSex Female)
    parseJSON (String "M")     = pure (JannoSex Male)
    parseJSON (String "U")     = pure (JannoSex Unknown)
    parseJSON v                = fail ("could not parse " ++ show v ++ " as JannoSex")

instance ToJSON JannoSex where
    -- this encodes directly to a bytestring Builder
    toJSON (JannoSex Female)   = String "F"
    toJSON (JannoSex Male)     = String "M"
    toJSON (JannoSex Unknown)  = String "U"

instance Show JannoSex where
    show (JannoSex Female)     = "F"
    show (JannoSex Male)       = "M"
    show (JannoSex Unknown)    = "U"

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
        | x == "C14"        = pure C14
        | x == "contextual" = pure Contextual
        | x == "modern"     = pure Modern
        | otherwise         = fail $ "Date_Type " ++ show x ++ " not in [C14, contextual, modern]"

instance Csv.ToField JannoDateType where
    toField C14             = "C14"
    toField Contextual      = "contextual"
    toField Modern          = "modern"

instance Show JannoDateType where
    show C14                = "C14"
    show Contextual         = "contextual"
    show Modern             = "modern"

-- |A datatype to represent Capture_Type in a janno file
data JannoCaptureType = Shotgun
    | A1240K
    | OtherCapture
    | ReferenceGenome
    deriving (Eq, Ord, Generic)

instance ToJSON JannoCaptureType where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JannoCaptureType

instance Csv.FromField JannoCaptureType where
    parseField x
        | x == "Shotgun"         = pure Shotgun
        | x == "1240K"           = pure A1240K
        | x == "OtherCapture"    = pure OtherCapture
        | x == "ReferenceGenome" = pure ReferenceGenome
        | otherwise              = fail $ "Capture_Type " ++ show x ++ 
                                          " not in [Shotgun, 1240K, OtherCapture, ReferenceGenome]"

instance Csv.ToField JannoCaptureType where
    toField Shotgun              = "Shotgun"
    toField A1240K               = "1240K"
    toField OtherCapture         = "OtherCapture"
    toField ReferenceGenome      = "ReferenceGenome"

instance Show JannoCaptureType where
    show Shotgun                 = "Shotgun"
    show A1240K                  = "1240K"
    show OtherCapture            = "OtherCapture"
    show ReferenceGenome         = "ReferenceGenome"

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
        | otherwise      = fail $ "Genotype_Ploidy " ++ show x ++ " not in [diploid, haploid]"

instance Csv.ToField JannoGenotypePloidy where
    toField Diploid      = "diploid"
    toField Haploid      = "haploid"

instance Show JannoGenotypePloidy where
    show Diploid         = "diploid"
    show Haploid         = "haploid"

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
        | x == "half"  = pure Half
        | x == "plus"  = pure Plus
        | x == "mixed" = pure Mixed
        | otherwise    = fail $ "UDG " ++ show x ++ " not in [minus, half, plus, mixed]"

instance Csv.ToField JannoUDG where
    toField Minus      = "minus"
    toField Half       = "half"
    toField Plus       = "plus"
    toField Mixed      = "mixed"

instance Show JannoUDG where
    show Minus         = "minus"
    show Half          = "half"
    show Plus          = "plus"
    show Mixed         = "mixed"

-- |A datatype to represent Library_Built in a janno file
data JannoLibraryBuilt = DS
    | SS
    | Other
    deriving (Eq, Ord, Generic)

instance Csv.FromField JannoLibraryBuilt where
    parseField x
        | x == "ds"    = pure DS
        | x == "ss"    = pure SS
        | x == "other" = pure Other
        | otherwise    = fail $ "Library_Built " ++ show x ++ " not in [ds, ss, other]"

instance Csv.ToField JannoLibraryBuilt where
    toField DS         = "ds"
    toField SS         = "ss"
    toField Other      = "other"

instance Show JannoLibraryBuilt where
    show DS            = "ds"
    show SS            = "ss"
    show Other         = "other"

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
        then fail $ "Latitude " ++ show x ++ " not between -90 and 90"
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
        then fail $ "Longitude " ++ show x ++ " not between -180 and 180"
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
        then fail $ "Percent value " ++ show x ++ " not between 0 and 100"
        else pure (Percent val)

instance Csv.ToField Percent where
    toField (Percent x) = Csv.toField x

instance Show Percent where
    show (Percent x) = show x

-- | A datatype to represent URIs in a janno file
newtype JURI = JURI String
    deriving (Eq, Ord, Generic)

instance ToJSON JURI where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON JURI

instance Csv.FromField JURI where
    parseField x = do
        val <- Csv.parseField x
        if not $ isURI val
        then fail $ "URI " ++ show x ++ " not well structured"
        else pure $ JURI val

instance Csv.ToField JURI where
    toField x = Csv.toField $ show x

instance Show JURI where
    show (JURI x) = x

-- | A general datatype for janno list columns
newtype JannoList a = JannoList {getJannoList :: [a]} 
    deriving (Eq, Ord, Generic, Show)

type JannoStringList = JannoList String
type JannoIntList = JannoList Int

instance (Csv.ToField a) => Csv.ToField (JannoList a) where
    toField = Csv.toField . intercalate ";" . map (read . show . Csv.toField) . getJannoList 

instance (Csv.FromField a) => Csv.FromField (JannoList a) where
    parseField x = do
        fieldStr <- Csv.parseField x
        let subStrings = Bchs.splitWith (==';') fieldStr
        fmap JannoList . mapM Csv.parseField $ subStrings

instance (ToJSON a) => ToJSON (JannoList a) where
    toJSON (JannoList x) = toJSON x

instance (FromJSON a) => FromJSON (JannoList a) where
    parseJSON v = JannoList <$> parseJSON v

-- |A datatype to represent Relationship degree lists in a janno file
type JannoRelationDegreeList = JannoList RelationDegree

data RelationDegree = Identical | First | Second | ThirdToFifth | SixthToTenth | Unrelated | OtherDegree
    deriving (Eq, Ord, Generic)

instance ToJSON RelationDegree where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON RelationDegree

instance Csv.FromField RelationDegree where
    parseField x
        | x == "identical"    = pure Identical
        | x == "first"        = pure First
        | x == "second"       = pure Second
        | x == "thirdToFifth" = pure ThirdToFifth
        | x == "sixthToTenth" = pure SixthToTenth
        | x == "unrelated"    = pure Unrelated -- this should be omitted in the documentation
                                               -- relations of type "unrelated" don't have to be
                                               -- listed explicitly
        | x == "other"        = pure OtherDegree
        | otherwise           = fail $ "Relation degree " ++ show x ++ 
                                       " not in [identical, first, second, thirdToFifth, sixthToTenth, other]"

instance Csv.ToField RelationDegree where
    toField Identical         = "identical"
    toField First             = "first"
    toField Second            = "second"
    toField ThirdToFifth      = "thirdToFifth"
    toField SixthToTenth      = "sixthToTenth"
    toField Unrelated         = "unrelated"
    toField OtherDegree       = "other"

instance Show RelationDegree where
    show Identical            = "identical"
    show First                = "first"
    show Second               = "second"
    show ThirdToFifth         = "thirdToFifth"
    show SixthToTenth         = "sixthToTenth"
    show Unrelated            = "unrelated"
    show OtherDegree          = "other"

-- | A data type to represent a sample/janno file row
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/janno_columns.tsv
-- for more details
data JannoRow = JannoRow
    { jPoseidonID                   :: String
    , jAlternativeIDs               :: Maybe JannoStringList
    , jRelationTo                   :: Maybe JannoStringList
    , jRelationDegree               :: Maybe JannoRelationDegreeList
    , jRelationType                 :: Maybe JannoStringList
    , jRelationNote                 :: Maybe String
    , jCollectionID                 :: Maybe String
    , jSourceTissue                 :: Maybe JannoStringList
    , jCountry                      :: Maybe String
    , jLocation                     :: Maybe String
    , jSite                         :: Maybe String
    , jLatitude                     :: Maybe Latitude
    , jLongitude                    :: Maybe Longitude
    , jDateC14Labnr                 :: Maybe JannoStringList
    , jDateC14UncalBP               :: Maybe JannoIntList
    , jDateC14UncalBPErr            :: Maybe JannoIntList
    , jDateBCADMedian               :: Maybe Int
    , jDateBCADStart                :: Maybe Int
    , jDateBCADStop                 :: Maybe Int
    , jDateType                     :: Maybe JannoDateType
    , jDateNote                     :: Maybe String
    , jNrLibraries                  :: Maybe Int
    , jCaptureType                  :: Maybe (JannoList JannoCaptureType)
    , jGenotypePloidy               :: Maybe JannoGenotypePloidy
    , jGroupName                    :: JannoStringList
    , jGeneticSex                   :: JannoSex
    , jNrSNPs                       :: Maybe Int
    , jCoverageOnTargets            :: Maybe Double
    , jMTHaplogroup                 :: Maybe String
    , jYHaplogroup                  :: Maybe String
    , jEndogenous                   :: Maybe Percent
    , jUDG                          :: Maybe JannoUDG
    , jLibraryBuilt                 :: Maybe JannoLibraryBuilt
    , jDamage                       :: Maybe Percent
    , jContamination                :: Maybe JannoStringList
    , jContaminationErr             :: Maybe JannoStringList
    , jContaminationMeas            :: Maybe JannoStringList
    , jContaminationNote            :: Maybe String
    , jGeneticSourceAccessionIDs    :: Maybe JannoStringList
    , jDataPreparationPipelineURL   :: Maybe JURI
    , jPrimaryContact               :: Maybe String
    , jPublication                  :: Maybe JannoStringList
    , jComments                     :: Maybe String
    , jKeywords                     :: Maybe JannoStringList
    }
    deriving (Show, Eq, Generic)

instance ToJSON JannoRow where
    toEncoding = genericToEncoding (defaultOptions {omitNothingFields = True})

instance FromJSON JannoRow

instance Csv.FromNamedRecord JannoRow where
    parseNamedRecord m = JannoRow 
        <$> filterLookup         m "Poseidon_ID"
        <*> filterLookupOptional m "Alternative_IDs"
        <*> filterLookupOptional m "Relation_To"
        <*> filterLookupOptional m "Relation_Degree"
        <*> filterLookupOptional m "Relation_Type"
        <*> filterLookupOptional m "Relation_Note"
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
        <*> filterLookupOptional m "Date_Note"
        <*> filterLookupOptional m "Nr_Libraries"
        <*> filterLookupOptional m "Capture_Type"
        <*> filterLookupOptional m "Genotype_Ploidy"
        <*> filterLookup         m "Group_Name"
        <*> filterLookup         m "Genetic_Sex"
        <*> filterLookupOptional m "Nr_SNPs"
        <*> filterLookupOptional m "Coverage_on_Target_SNPs"
        <*> filterLookupOptional m "MT_Haplogroup"
        <*> filterLookupOptional m "Y_Haplogroup"
        <*> filterLookupOptional m "Endogenous"
        <*> filterLookupOptional m "UDG"
        <*> filterLookupOptional m "Library_Built"
        <*> filterLookupOptional m "Damage"
        <*> filterLookupOptional m "Contamination"
        <*> filterLookupOptional m "Contamination_Err"
        <*> filterLookupOptional m "Contamination_Meas"
        <*> filterLookupOptional m "Contamination_Note"
        <*> filterLookupOptional m "Genetic_Source_Accession_IDs"
        <*> filterLookupOptional m "Data_Preparation_Pipeline_URL"
        <*> filterLookupOptional m "Primary_Contact"
        <*> filterLookupOptional m "Publication"
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
    toNamedRecord j = Csv.namedRecord [
          "Poseidon_ID"                     Csv..= jPoseidonID j
        , "Alternative_IDs"                 Csv..= jAlternativeIDs j
        , "Relation_To"                     Csv..= jRelationTo j
        , "Relation_Degree"                 Csv..= jRelationDegree j
        , "Relation_Type"                   Csv..= jRelationType j
        , "Relation_Note"                   Csv..= jRelationNote j
        , "Collection_ID"                   Csv..= jCollectionID j
        , "Source_Tissue"                   Csv..= jSourceTissue j
        , "Country"                         Csv..= jCountry j
        , "Location"                        Csv..= jLocation j
        , "Site"                            Csv..= jSite j
        , "Latitude"                        Csv..= jLatitude j
        , "Longitude"                       Csv..= jLongitude j
        , "Date_C14_Labnr"                  Csv..= jDateC14Labnr j
        , "Date_C14_Uncal_BP"               Csv..= jDateC14UncalBP j
        , "Date_C14_Uncal_BP_Err"           Csv..= jDateC14UncalBPErr j
        , "Date_BC_AD_Median"               Csv..= jDateBCADMedian j
        , "Date_BC_AD_Start"                Csv..= jDateBCADStart j
        , "Date_BC_AD_Stop"                 Csv..= jDateBCADStop j
        , "Date_Type"                       Csv..= jDateType j
        , "Date_Note"                       Csv..= jDateNote j
        , "Nr_Libraries"                    Csv..= jNrLibraries j
        , "Capture_Type"                    Csv..= jCaptureType j
        , "Genotype_Ploidy"                 Csv..= jGenotypePloidy j
        , "Group_Name"                      Csv..= jGroupName j
        , "Genetic_Sex"                     Csv..= jGeneticSex j
        , "Nr_SNPs"                         Csv..= jNrSNPs j
        , "Coverage_on_Target_SNPs"         Csv..= jCoverageOnTargets j
        , "MT_Haplogroup"                   Csv..= jMTHaplogroup j
        , "Y_Haplogroup"                    Csv..= jYHaplogroup j
        , "Endogenous"                      Csv..= jEndogenous j
        , "UDG"                             Csv..= jUDG j
        , "Library_Built"                   Csv..= jLibraryBuilt j
        , "Damage"                          Csv..= jDamage j
        , "Contamination"                   Csv..= jContamination j
        , "Contamination_Err"               Csv..= jContaminationErr j
        , "Contamination_Meas"              Csv..= jContaminationMeas j
        , "Contamination_Note"              Csv..= jContaminationNote j
        , "Genetic_Source_Accession_IDs"    Csv..= jGeneticSourceAccessionIDs j
        , "Data_Preparation_Pipeline_URL"   Csv..= jDataPreparationPipelineURL j
        , "Primary_Contact"                 Csv..= jPrimaryContact j
        , "Publication"                     Csv..= jPublication j
        , "Note"                            Csv..= jComments j
        , "Keywords"                        Csv..= jKeywords j
        ]

instance Csv.DefaultOrdered JannoRow where
    headerOrder _ = Csv.header jannoHeader

-- This header also defines the output column order when writing to csv!
jannoHeader :: [Bchs.ByteString]
jannoHeader = [
      "Poseidon_ID"
    , "Genetic_Sex"
    , "Group_Name"
    , "Alternative_IDs"
    , "Relation_To"
    , "Relation_Degree"
    , "Relation_Type"
    , "Relation_Note"
    , "Collection_ID"
    , "Country"
    , "Location"
    , "Site"
    , "Latitude"
    , "Longitude"
    , "Date_Type"
    , "Date_C14_Labnr"
    , "Date_C14_Uncal_BP"
    , "Date_C14_Uncal_BP_Err"
    , "Date_BC_AD_Start"
    , "Date_BC_AD_Median"
    , "Date_BC_AD_Stop"
    , "Date_Note"
    , "MT_Haplogroup"
    , "Y_Haplogroup"
    , "Source_Tissue"
    , "Nr_Libraries"
    , "Capture_Type"
    , "UDG"
    , "Library_Built"
    , "Genotype_Ploidy"
    , "Data_Preparation_Pipeline_URL"
    , "Endogenous"
    , "Nr_SNPs"
    , "Coverage_on_Target_SNPs"
    , "Damage"
    , "Contamination"
    , "Contamination_Err"
    , "Contamination_Meas"
    , "Contamination_Note"
    , "Genetic_Source_Accession_IDs"
    , "Primary_Contact"
    , "Publication"
    , "Note"
    , "Keywords"
    ]

jannoHeaderString :: [String]
jannoHeaderString = map Bchs.unpack jannoHeader

-- | A function to create empty janno rows for a set of individuals
createMinimalJanno :: [EigenstratIndEntry] -> [JannoRow]
createMinimalJanno [] = []
createMinimalJanno xs = map createMinimalSample xs

-- | A function to create an empty janno row for an individual
createMinimalSample :: EigenstratIndEntry -> JannoRow
createMinimalSample (EigenstratIndEntry id_ sex pop) =
    JannoRow { 
          jPoseidonID                   = id_
        , jAlternativeIDs               = Nothing
        , jRelationTo                   = Nothing
        , jRelationDegree               = Nothing
        , jRelationType                 = Nothing
        , jRelationNote                 = Nothing
        , jCollectionID                 = Nothing
        , jSourceTissue                 = Nothing
        , jCountry                      = Nothing
        , jLocation                     = Nothing
        , jSite                         = Nothing
        , jLatitude                     = Nothing
        , jLongitude                    = Nothing
        , jDateC14Labnr                 = Nothing
        , jDateC14UncalBP               = Nothing
        , jDateC14UncalBPErr            = Nothing
        , jDateBCADMedian               = Nothing
        , jDateBCADStart                = Nothing
        , jDateBCADStop                 = Nothing
        , jDateType                     = Nothing
        , jDateNote                     = Nothing
        , jNrLibraries                  = Nothing
        , jCaptureType                  = Nothing
        , jGenotypePloidy               = Nothing
        , jGroupName                    = JannoList [pop]
        , jGeneticSex                   = JannoSex sex
        , jNrSNPs                       = Nothing
        , jCoverageOnTargets            = Nothing
        , jMTHaplogroup                 = Nothing
        , jYHaplogroup                  = Nothing
        , jEndogenous                   = Nothing
        , jUDG                          = Nothing
        , jLibraryBuilt                 = Nothing
        , jDamage                       = Nothing
        , jContamination                = Nothing
        , jContaminationErr             = Nothing
        , jContaminationMeas            = Nothing
        , jContaminationNote            = Nothing
        , jGeneticSourceAccessionIDs    = Nothing
        , jDataPreparationPipelineURL   = Nothing
        , jPrimaryContact               = Nothing
        , jPublication                  = Nothing
        , jComments                     = Nothing
        , jKeywords                     = Nothing
    }

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
        jannoFileRowsWithNumberFiltered = filter (\(_, y) -> y /= Bch.empty) jannoFileRowsWithNumber
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
            hPutStrLn stderr $ "Additional columns: " ++ 
                -- for each additional column a standard column is suggested: "Countro (Country?)"
                intercalate ", " (zipWith (\x y -> x ++ " (" ++ y ++ "?)") 
                    additional_columns (findSimilarNames missing_columns additional_columns))
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

findSimilarNames :: [String] -> [String] -> [String]
findSimilarNames reference toLookUp = map (findSimilar reference) toLookUp
    where
        findSimilar ::  [String] -> String -> String
        findSimilar ref x = 
            let dists = map (\y -> x `editDistance` y) ref
            in ref !! fromJust (elemIndex (minimum dists) dists)

-- | A function to load one row of a janno file
readJannoFileRow :: FilePath -> (Int, Bch.ByteString) -> IO (Either PoseidonException JannoRow)
readJannoFileRow jannoPath (lineNumber, row) = do
    case Csv.decodeByNameWith decodingOptions row of
        Left e -> do
            return $ Left $ PoseidonJannoRowException jannoPath lineNumber $ removeUselessSuffix e
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

removeUselessSuffix :: String -> String
removeUselessSuffix = unpack . replace " at \"\"" "" . pack

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

-- Janno consistency checks

checkJannoConsistency :: FilePath -> [JannoRow] -> Either PoseidonException [JannoRow]
checkJannoConsistency jannoPath xs
    | not $ checkIndividualUnique xs = Left $ PoseidonJannoConsistencyException jannoPath
        "The Poseidon_IDs are not unique"
    | otherwise = Right xs

checkIndividualUnique :: [JannoRow] -> Bool
checkIndividualUnique x = length x == length (nub $ map jPoseidonID x)

checkJannoRowConsistency :: FilePath -> Int -> JannoRow -> Either PoseidonException JannoRow
checkJannoRowConsistency jannoPath row x
    | not $ checkMandatoryStringNotEmpty x = Left $ PoseidonJannoRowException jannoPath row
          "The mandatory columns Poseidon_ID and Group_Name contain empty values"
    | not $ checkC14ColsConsistent x = Left $ PoseidonJannoRowException jannoPath row
          "The Date_* columns are not consistent"
    | not $ checkContamColsConsistent x = Left $ PoseidonJannoRowException jannoPath row
          "The Contamination_* columns are not consistent"
    | not $ checkRelationColsConsistent x = Left $ PoseidonJannoRowException jannoPath row
          "The Relation_* columns are not consistent"
    | otherwise = Right x

checkMandatoryStringNotEmpty :: JannoRow -> Bool
checkMandatoryStringNotEmpty x =
    (not . null . jPoseidonID $ x)
    && (not . null . getJannoList . jGroupName $ x)
    && (not . null . head . getJannoList . jGroupName $ x)

getCellLength :: Maybe (JannoList a) -> Int 
getCellLength = maybe 0 (length . getJannoList)

allEqual :: Eq a => [a] -> Bool
allEqual x = length (nub x) == 1

checkC14ColsConsistent :: JannoRow -> Bool
checkC14ColsConsistent x =
    let lLabnr          = getCellLength $ jDateC14Labnr x
        lUncalBP        = getCellLength $ jDateC14UncalBP x
        lUncalBPErr     = getCellLength $ jDateC14UncalBPErr x
        shouldBeTypeC14 = lUncalBP > 0
        isTypeC14       = jDateType x == Just C14
    in
        (lLabnr == 0 || lUncalBP == 0 || lLabnr == lUncalBP)
        && (lLabnr == 0 || lUncalBPErr == 0 || lLabnr == lUncalBPErr)
        && lUncalBP == lUncalBPErr
        && (not shouldBeTypeC14 || isTypeC14)

checkContamColsConsistent :: JannoRow -> Bool
checkContamColsConsistent x =
  let lContamination      = getCellLength $ jContamination x
      lContaminationErr   = getCellLength $ jContaminationErr x
      lContaminationMeas  = getCellLength $ jContaminationMeas x
  in allEqual [lContamination, lContaminationErr, lContaminationMeas]
  
checkRelationColsConsistent :: JannoRow -> Bool
checkRelationColsConsistent x =
  let lRelationTo = getCellLength $ jRelationTo x
      lRelationDegree = getCellLength $ jRelationDegree x
      lRelationType = getCellLength $ jRelationType x
  in allEqual [lRelationTo, lRelationType, lRelationDegree]
     || (allEqual [lRelationTo, lRelationDegree] && isNothing (jRelationType x))
