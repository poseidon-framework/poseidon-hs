{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.Janno (
    JannoRow(..),
    JannoSex (..),
    JannoList (..),
    Sex (..),
    BCADAge (..),
    JannoCountry (..),
    makeJannoCountryUnsafe,
    Latitude (..),
    Longitude (..),
    JannoDateType (..),
    JannoCaptureType (..),
    JannoGenotypePloidy (..),
    Percent (..),
    JannoUDG (..),
    JURI (..),
    RelationDegree (..),
    JannoLibraryBuilt (..),
    AccessionID (..),
    makeAccessionID,
    writeJannoFile,
    readJannoFile,
    createMinimalJanno,
    jannoHeaderString,
    CsvNamedRecord (..),
    JannoRows (..),
    JannoStringList,
    filterLookup,
    getCsvNR,
    encodingOptions,
    decodingOptions,
    explicitNA,
    removeUselessSuffix
) where

import           Poseidon.Utils                       (PoseidonException (..),
                                                       PoseidonIO, logDebug,
                                                       renderPoseidonException)


import           Control.Applicative                  (empty)
import           Control.Exception                    (throwIO)
import           Control.Monad                        (unless, when)
import           Control.Monad.IO.Class               (liftIO)
import           Country                              (Country, alphaTwoUpper,
                                                       decodeAlphaTwo)
import           Data.Aeson                           (FromJSON, Options (..),
                                                       ToJSON, Value (..),
                                                       defaultOptions,
                                                       genericToEncoding,
                                                       parseJSON, toEncoding,
                                                       toJSON, withText)
import           Data.Aeson.Types                     (emptyObject)
import           Data.Bifunctor                       (second)
import qualified Data.ByteString.Char8                as Bchs
import qualified Data.ByteString.Lazy.Char8           as Bch
import           Data.Char                            (isSpace, ord)
import qualified Data.Csv                             as Csv
import           Data.Either                          (lefts, rights)
import qualified Data.HashMap.Strict                  as HM
import           Data.List                            (elemIndex, foldl',
                                                       intercalate, nub, sort,
                                                       (\\))
import           Data.Maybe                           (fromJust, isNothing)
import           Data.Text                            (pack, replace, unpack)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Vector                          as V
import           GHC.Generics                         (Generic)
import           Network.URI                          (isURI)
import           Options.Applicative.Help.Levenshtein (editDistance)
import           SequenceFormats.Eigenstrat           (EigenstratIndEntry (..),
                                                       Sex (..))
import qualified Text.Regex.TDFA                      as Reg

-- | A datatype for genetic sex
newtype JannoSex = JannoSex { sfSex :: Sex }
    deriving (Eq)

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

makeJannoSex :: MonadFail m => String -> m JannoSex
makeJannoSex x
    | x == "F"  = pure (JannoSex Female)
    | x == "M"  = pure (JannoSex Male)
    | x == "U"  = pure (JannoSex Unknown)
    | otherwise = fail $ "Sex " ++ show x ++ " not in [F, M, U]"

instance Csv.ToField JannoSex where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoSex where
    parseField x = Csv.parseField x >>= makeJannoSex
instance ToJSON JannoSex where
    toJSON x  = String $ T.pack $ show x
instance FromJSON JannoSex where
    parseJSON = withText "JannoSex" (makeJannoSex . T.unpack)

-- | A datatype for BC-AD ages
newtype BCADAge =
        BCADAge Int
    deriving (Eq, Ord, Generic)

instance Show BCADAge where
    show (BCADAge x) = show x

makeBCADAge :: MonadFail m => Int -> m BCADAge
makeBCADAge x =
    let curYear = 2023 -- the current year
    in if x >= curYear
       then fail $ "Age " ++ show x ++ " later than " ++ show curYear ++ ", which is impossible. " ++
                   "Did you accidentally enter a BP date?"
      else pure (BCADAge x)

instance Csv.ToField BCADAge where
    toField (BCADAge x) = Csv.toField x
instance Csv.FromField BCADAge where
    parseField x = Csv.parseField x >>= makeBCADAge
instance ToJSON BCADAge where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON BCADAge-- where
    --parseJSON = withScientific "BCADAge" $ \n ->
    --    case toBoundedInteger n of
    --        Nothing -> fail $ "Number" ++ show n ++ "doesn't fit into a bounded integer."
    --        Just x -> makeBCADAge x

-- |A datatype to represent Date_Type in a janno file
data JannoDateType =
      C14
    | Contextual
    | Modern
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show JannoDateType where
    show C14        = "C14"
    show Contextual = "contextual"
    show Modern     = "modern"

makeJannoDateType :: MonadFail m => String -> m JannoDateType
makeJannoDateType x
    | x == "C14"        = pure C14
    | x == "contextual" = pure Contextual
    | x == "modern"     = pure Modern
    | otherwise         = fail $ "Date_Type " ++ show x ++ " not in [C14, contextual, modern]"

instance Csv.ToField JannoDateType where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoDateType where
    parseField x = Csv.parseField x >>= makeJannoDateType
instance ToJSON JannoDateType where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON JannoDateType-- where
    --parseJSON = withText "JannoDateType" (makeJannoDateType . T.unpack)

-- |A datatype to represent Capture_Type in a janno file
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

instance Show JannoCaptureType where
    show Shotgun            = "Shotgun"
    show A1240K             = "1240K"
    show ArborComplete      = "ArborComplete"
    show ArborPrimePlus     = "ArborPrimePlus"
    show ArborAncestralPlus = "ArborAncestralPlus"
    show TwistAncientDNA    = "TwistAncientDNA"
    show OtherCapture       = "OtherCapture"
    show ReferenceGenome    = "ReferenceGenome"

makeJannoCaptureType :: MonadFail m => String -> m JannoCaptureType
makeJannoCaptureType x
    | x == "Shotgun"            = pure Shotgun
    | x == "1240K"              = pure A1240K
    | x == "ArborComplete"      = pure ArborComplete
    | x == "ArborPrimePlus"     = pure ArborPrimePlus
    | x == "ArborAncestralPlus" = pure ArborAncestralPlus
    | x == "TwistAncientDNA"    = pure TwistAncientDNA
    | x == "OtherCapture"       = pure OtherCapture
    | x == "ReferenceGenome"    = pure ReferenceGenome
    | otherwise                 = fail $ "Capture_Type " ++ show x ++
                                      " not in [Shotgun, 1240K, ArborComplete, ArborPrimePlus, ArborAncestralPlus, TwistAncientDNA, OtherCapture, ReferenceGenome]"

instance Csv.ToField JannoCaptureType where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoCaptureType where
    parseField x = Csv.parseField x >>= makeJannoCaptureType
instance ToJSON JannoCaptureType where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON JannoCaptureType-- where
    --parseJSON = withText "JannoCaptureType" (makeJannoCaptureType . T.unpack)

-- |A datatype to represent Genotype_Ploidy in a janno file
data JannoGenotypePloidy =
      Diploid
    | Haploid
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show JannoGenotypePloidy where
    show Diploid = "diploid"
    show Haploid = "haploid"

makeJannoGenotypePloidy :: MonadFail m => String -> m JannoGenotypePloidy
makeJannoGenotypePloidy x
    | x == "diploid" = pure Diploid
    | x == "haploid" = pure Haploid
    | otherwise      = fail $ "Genotype_Ploidy " ++ show x ++ " not in [diploid, haploid]"

instance Csv.ToField JannoGenotypePloidy where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoGenotypePloidy where
    parseField x = Csv.parseField x >>= makeJannoGenotypePloidy
instance ToJSON JannoGenotypePloidy where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON JannoGenotypePloidy-- where
    --parseJSON = withText "JannoGenotypePloidy" (makeJannoGenotypePloidy . T.unpack)

-- |A datatype to represent UDG in a janno file
data JannoUDG =
      Minus
    | Half
    | Plus
    | Mixed
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show JannoUDG where
    show Minus = "minus"
    show Half  = "half"
    show Plus  = "plus"
    show Mixed = "mixed"

makeJannoUDG :: MonadFail m => String -> m JannoUDG
makeJannoUDG x
    | x == "minus" = pure Minus
    | x == "half"  = pure Half
    | x == "plus"  = pure Plus
    | x == "mixed" = pure Mixed
    | otherwise    = fail $ "UDG " ++ show x ++ " not in [minus, half, plus, mixed, other]"

instance Csv.ToField JannoUDG where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoUDG where
    parseField x = Csv.parseField x >>= makeJannoUDG
instance ToJSON JannoUDG where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON JannoUDG-- where
    --parseJSON = withText "JannoUDG" (makeJannoUDG . T.unpack)

-- |A datatype to represent Library_Built in a janno file
data JannoLibraryBuilt =
      DS
    | SS
    | MixedSSDS
    | Other -- the "other" option is deprecated and should be removed at some point
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show JannoLibraryBuilt where
    show DS        = "ds"
    show SS        = "ss"
    show MixedSSDS = "mixed"
    show Other     = "other"

makeJannoLibraryBuilt :: MonadFail m => String -> m JannoLibraryBuilt
makeJannoLibraryBuilt x
    | x == "ds"    = pure DS
    | x == "ss"    = pure SS
    | x == "mixed" = pure MixedSSDS
    | x == "other" = pure Other
    | otherwise    = fail $ "Library_Built " ++ show x ++ " not in [ds, ss, mixed]"

instance Csv.ToField JannoLibraryBuilt where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoLibraryBuilt where
    parseField x = Csv.parseField x >>= makeJannoLibraryBuilt
instance ToJSON JannoLibraryBuilt where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON JannoLibraryBuilt --where
    --parseJSON = withText "JannoLibraryBuilt" (makeJannoLibraryBuilt . T.unpack)

-- | A datatype for countries in ISO-alpha2 code format
newtype JannoCountry = JannoCountry Country
    deriving (Eq, Ord)

instance Show JannoCountry where
    show (JannoCountry x) = T.unpack $ alphaTwoUpper x

makeJannoCountryUnsafe :: String -> JannoCountry
makeJannoCountryUnsafe x =
    case decodeAlphaTwo (T.pack x) of
        Just c  -> JannoCountry c
        Nothing -> error $ x ++ " is not a valid ISO-alpha2 code describing an existing country"

makeJannoCountry :: MonadFail m => String -> m JannoCountry
makeJannoCountry x =
    case decodeAlphaTwo (T.pack x) of
        Just c  -> pure $ JannoCountry c
        Nothing -> fail $ x ++ " is not a valid ISO-alpha2 code describing an existing country"

instance Csv.ToField JannoCountry where
    toField x = Csv.toField $ show x
instance Csv.FromField JannoCountry where
    parseField x = Csv.parseField x >>= makeJannoCountry
instance ToJSON JannoCountry where
    toJSON x  = String $ T.pack $ show x
instance FromJSON JannoCountry where
    parseJSON = withText "JannoCountry" (makeJannoCountry . T.unpack)

-- | A datatype for Latitudes
newtype Latitude =
        Latitude Double
    deriving (Eq, Ord, Generic)

instance Show Latitude where
    show (Latitude x) = show x

makeLatitude :: MonadFail m => Double -> m Latitude
makeLatitude x
    | x >= -90 && x <= 90 = pure (Latitude x)
    | otherwise           = fail $ "Latitude " ++ show x ++ " not between -90 and 90"

instance Csv.ToField Latitude where
    toField (Latitude x) = Csv.toField x
instance Csv.FromField Latitude where
    parseField x = Csv.parseField x >>= makeLatitude
instance ToJSON Latitude where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Latitude-- where
    --parseJSON = withScientific "Latitude" $ \n -> (makeLatitude . toRealFloat) n

-- | A datatype for Longitudes
newtype Longitude =
        Longitude Double
    deriving (Eq, Ord, Generic)

instance Show Longitude where
    show (Longitude x) = show x

makeLongitude :: MonadFail m => Double -> m Longitude
makeLongitude x
    | x >= -180 && x <= 180 = pure (Longitude x)
    | otherwise             = fail $ "Longitude " ++ show x ++ " not between -180 and 180"

instance Csv.ToField Longitude where
    toField (Longitude x) = Csv.toField x
instance Csv.FromField Longitude where
    parseField x = Csv.parseField x >>= makeLongitude
instance ToJSON Longitude where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Longitude-- where
    --parseJSON = withScientific "Longitude" $ \n -> (makeLongitude . toRealFloat) n

-- | A datatype for Percent values
newtype Percent =
        Percent Double
    deriving (Eq, Ord, Generic)

instance Show Percent where
    show (Percent x) = show x

makePercent :: MonadFail m => Double -> m Percent
makePercent x
    | x >= 0 && x <= 100 = pure (Percent x)
    | otherwise          = fail $ "Percentage " ++ show x ++ " not between 0 and 100"

instance Csv.ToField Percent where
    toField (Percent x) = Csv.toField x
instance Csv.FromField Percent where
    parseField x = Csv.parseField x >>= makePercent
instance ToJSON Percent where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Percent-- where
    --parseJSON = withScientific "Percent" $ \n -> (makePercent . toRealFloat) n

-- | A datatype to represent URIs in a janno file
newtype JURI =
        JURI String
    deriving (Eq, Ord, Generic)

instance Show JURI where
    show (JURI x) = x

makeJURI :: MonadFail m => String -> m JURI
makeJURI x
    | isURI x   = pure $ JURI x
    | otherwise = fail $ "URI " ++ show x ++ " not well structured"

instance Csv.ToField JURI where
    toField x = Csv.toField $ show x
instance Csv.FromField JURI where
    parseField x = Csv.parseField x >>= makeJURI
instance ToJSON JURI where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON JURI-- where
    --parseJSON = withText "JURI" (makeJURI . T.unpack)

-- |A datatype to represent Relationship degree lists in a janno file
type JannoRelationDegreeList = JannoList RelationDegree

data RelationDegree =
      Identical
    | First
    | Second
    | ThirdToFifth
    | SixthToTenth
    | Unrelated
    | OtherDegree
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Show RelationDegree where
    show Identical    = "identical"
    show First        = "first"
    show Second       = "second"
    show ThirdToFifth = "thirdToFifth"
    show SixthToTenth = "sixthToTenth"
    show Unrelated    = "unrelated"
    show OtherDegree  = "other"

makeRelationDegree :: MonadFail m => String -> m RelationDegree
makeRelationDegree x
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
    toField x = Csv.toField $ show x
instance Csv.FromField RelationDegree where
    parseField x = Csv.parseField x >>= makeRelationDegree
instance ToJSON RelationDegree where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON RelationDegree-- where
    --parseJSON = withText "RelationDegree" (makeRelationDegree . T.unpack)

-- |A datatype to represent AccessionIDs in a janno file
data AccessionID =
      INSDCProject String
    | INSDCStudy String
    | INSDCBioSample String
    | INSDCSample String
    | INSDCExperiment String
    | INSDCRun String
    | INSDCAnalysis String
    | OtherID String
    deriving (Eq, Ord, Generic)

instance Show AccessionID where
    show (INSDCProject x)    = x
    show (INSDCStudy x)      = x
    show (INSDCBioSample x)  = x
    show (INSDCSample x)     = x
    show (INSDCExperiment x) = x
    show (INSDCRun x)        = x
    show (INSDCAnalysis x)   = x
    show (OtherID x)         = x

-- the patterns are documented at:
-- https://ena-docs.readthedocs.io/en/latest/submit/general-guide/accessions.html
makeAccessionID :: MonadFail m => String -> m AccessionID
makeAccessionID x
    | x Reg.=~ ("PRJ[EDN][A-Z][0-9]+"  :: String) = pure $ INSDCProject x
    | x Reg.=~ ("[EDS]RP[0-9]{6,}"     :: String) = pure $ INSDCStudy x
    | x Reg.=~ ("SAM[EDN][A-Z]?[0-9]+" :: String) = pure $ INSDCBioSample x
    | x Reg.=~ ("[EDS]RS[0-9]{6,}"     :: String) = pure $ INSDCSample x
    | x Reg.=~ ("[EDS]RX[0-9]{6,}"     :: String) = pure $ INSDCExperiment x
    | x Reg.=~ ("[EDS]RR[0-9]{6,}"     :: String) = pure $ INSDCRun x
    | x Reg.=~ ("[EDS]RZ[0-9]{6,}"     :: String) = pure $ INSDCAnalysis x
    | otherwise                                   = pure $ OtherID x

instance Csv.ToField AccessionID where
    toField x = Csv.toField $ show x
instance Csv.FromField AccessionID where
    parseField x = Csv.parseField x >>= makeAccessionID
instance ToJSON AccessionID where
    toEncoding = genericToEncoding defaultOptions
    --toEncoding x = text $ T.pack $ show x
instance FromJSON AccessionID-- where
    --parseJSON = withText "AccessionID" (makeAccessionID . T.unpack)

-- | A general datatype for janno list columns
newtype JannoList a = JannoList {getJannoList :: [a]}
    deriving (Eq, Ord, Generic, Show)

type JannoStringList = JannoList String
type JannoIntList = JannoList Int

instance (Csv.ToField a, Show a) => Csv.ToField (JannoList a) where
    toField x = Bchs.intercalate ";" $ map Csv.toField $ getJannoList x
instance (Csv.FromField a) => Csv.FromField (JannoList a) where
    parseField x = fmap JannoList . mapM Csv.parseField $ Bchs.splitWith (==';') x
instance (ToJSON a) => ToJSON (JannoList a) where
    toEncoding (JannoList x) = toEncoding x
instance (FromJSON a) => FromJSON (JannoList a) where
    parseJSON x
        | isAesonString x = JannoList . singleton <$> parseJSON x
        | otherwise = JannoList <$> parseJSON x
        where
            isAesonString (String _) = True
            isAesonString _          = False
            singleton a = [a]

-- | A datatype to collect additional, unpecified .janno file columns (a hashmap in cassava/Data.Csv)
newtype CsvNamedRecord = CsvNamedRecord Csv.NamedRecord deriving (Show, Eq, Generic)

getCsvNR :: CsvNamedRecord -> Csv.NamedRecord
getCsvNR (CsvNamedRecord x) = x

-- Aeson does not encode ByteStrings, so that's why we have to go through Text
instance ToJSON CsvNamedRecord where
    toJSON (CsvNamedRecord x) =
        let listOfBSTuples = HM.toList x
            listOfTextTuples = map (\(a,b) -> (T.decodeUtf8 a, T.decodeUtf8 b)) listOfBSTuples
        in toJSON listOfTextTuples
instance FromJSON CsvNamedRecord where
    parseJSON x
        | x == emptyObject = pure $ CsvNamedRecord $ HM.fromList []
        | otherwise = do
            listOfTextTuples <- parseJSON x -- :: [(T.Text, T.Text)]
            let listOfBSTuples = map (\(a,b) -> (T.encodeUtf8 a, T.encodeUtf8 b)) listOfTextTuples
            pure $ CsvNamedRecord $ HM.fromList listOfBSTuples

-- | A  data type to represent a janno file
newtype JannoRows = JannoRows [JannoRow]
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

instance ToJSON JannoRows where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON JannoRows

-- | A data type to represent a sample/janno file row
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/janno_columns.tsv
-- for more details
data JannoRow = JannoRow
    { jPoseidonID                 :: String
    , jAlternativeIDs             :: Maybe JannoStringList
    , jRelationTo                 :: Maybe JannoStringList
    , jRelationDegree             :: Maybe JannoRelationDegreeList
    , jRelationType               :: Maybe JannoStringList
    , jRelationNote               :: Maybe String
    , jCollectionID               :: Maybe String
    , jSourceTissue               :: Maybe JannoStringList
    , jCountry                    :: Maybe String
    , jCountryISO                 :: Maybe JannoCountry
    , jLocation                   :: Maybe String
    , jSite                       :: Maybe String
    , jLatitude                   :: Maybe Latitude
    , jLongitude                  :: Maybe Longitude
    , jDateC14Labnr               :: Maybe JannoStringList
    , jDateC14UncalBP             :: Maybe JannoIntList
    , jDateC14UncalBPErr          :: Maybe JannoIntList
    , jDateBCADMedian             :: Maybe BCADAge
    , jDateBCADStart              :: Maybe BCADAge
    , jDateBCADStop               :: Maybe BCADAge
    , jDateType                   :: Maybe JannoDateType
    , jDateNote                   :: Maybe String
    , jNrLibraries                :: Maybe Int
    , jLibraryNames               :: Maybe JannoStringList
    , jCaptureType                :: Maybe (JannoList JannoCaptureType)
    , jGenotypePloidy             :: Maybe JannoGenotypePloidy
    , jGroupName                  :: JannoStringList
    , jGeneticSex                 :: JannoSex
    , jNrSNPs                     :: Maybe Int
    , jCoverageOnTargets          :: Maybe Double
    , jMTHaplogroup               :: Maybe String
    , jYHaplogroup                :: Maybe String
    , jEndogenous                 :: Maybe Percent
    , jUDG                        :: Maybe JannoUDG
    , jLibraryBuilt               :: Maybe JannoLibraryBuilt
    , jDamage                     :: Maybe Percent
    , jContamination              :: Maybe JannoStringList
    , jContaminationErr           :: Maybe JannoStringList
    , jContaminationMeas          :: Maybe JannoStringList
    , jContaminationNote          :: Maybe String
    , jGeneticSourceAccessionIDs  :: Maybe (JannoList AccessionID)
    , jDataPreparationPipelineURL :: Maybe JURI
    , jPrimaryContact             :: Maybe String
    , jPublication                :: Maybe JannoStringList
    , jComments                   :: Maybe String
    , jKeywords                   :: Maybe JannoStringList
    , jAdditionalColumns          :: CsvNamedRecord
    }
    deriving (Show, Eq, Generic)

-- This header also defines the output column order when writing to csv!
-- When the order is changed, don't forget to also update the order in the survey module
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
    , "Country_ISO"
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
    , "Library_Names"
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

instance Csv.DefaultOrdered JannoRow where
    headerOrder _ = Csv.header jannoHeader

jannoHeaderString :: [String]
jannoHeaderString = map Bchs.unpack jannoHeader

-- This hashmap represents an empty janno file with all normal, specified columns
jannoRefHashMap :: HM.HashMap Bchs.ByteString ()
jannoRefHashMap = HM.fromList $ map (\x -> (x, ())) jannoHeader

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
        <*> filterLookupOptional m "Country_ISO"
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
        <*> filterLookupOptional m "Library_Names"
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
        -- beyond that read everything that is not in the set of defined variables
        -- as a separate hashmap
        <*> pure (CsvNamedRecord (m `HM.difference` jannoRefHashMap))

filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = maybe empty Csv.parseField . cleanInput $ HM.lookup name m

filterLookupOptional :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional m name = maybe (pure Nothing) Csv.parseField . cleanInput $ HM.lookup name m

cleanInput :: Maybe Bchs.ByteString -> Maybe Bchs.ByteString
cleanInput Nothing           = Nothing
cleanInput (Just rawInputBS) = transNA $ trimWS . removeNoBreakSpace $ rawInputBS
    where
        trimWS :: Bchs.ByteString -> Bchs.ByteString
        trimWS = Bchs.dropWhile isSpace . Bchs.dropWhileEnd isSpace
        removeNoBreakSpace :: Bchs.ByteString -> Bchs.ByteString
        removeNoBreakSpace x
            | not $ Bchs.isInfixOf "\194\160" x = x
            | otherwise = removeNoBreakSpace $ (\(a,b) -> a <> Bchs.drop 2 b) $ Bchs.breakSubstring "\194\160" x
            -- When a unicode string with the No-Break Space character is loaded, parsed
            -- and written by cassava (in encodeByNameWith) it is unexpectedly expanded:
            -- "MAMS-47224\194\160" becomes "MAMS-47224\195\130\194\160
            -- This was surprisingly hard to fix. We decided to remove No-Break Space chars
            -- entirely before parsing them.
            -- Here are some resources to see, which unicode characters are actually in a string:
            -- https://www.soscisurvey.de/tools/view-chars.php
            -- https://qaz.wtf/u/show.cgi
            -- https://onlineunicodetools.com/convert-unicode-to-bytes
            -- The following code removes the characters \194 and \160 independently.
            -- This breaks other unicode characters and therefore does not solve the problem
            --Bchs.filter (\y -> y /= '\194' && y /= '\160') x -- \160 is No-Break Space
            -- The following code allows to debug the issue more precisely
            --let !a = unsafePerformIO $ putStrLn $ show x
            --    b = ...
            --    !c = unsafePerformIO $ putStrLn $ show b
            --in b
        transNA :: Bchs.ByteString -> Maybe Bchs.ByteString
        transNA ""    = Nothing
        transNA "n/a" = Nothing
        transNA x     = Just x

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
        , "Country_ISO"                     Csv..= jCountryISO j
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
        , "Library_Names"                   Csv..= jLibraryNames j
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
        , jAlternativeIDs               = Nothing
        , jRelationTo                   = Nothing
        , jRelationDegree               = Nothing
        , jRelationType                 = Nothing
        , jRelationNote                 = Nothing
        , jCollectionID                 = Nothing
        , jSourceTissue                 = Nothing
        , jCountry                      = Nothing
        , jCountryISO                   = Nothing
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
        , jLibraryNames                 = Nothing
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
        mapM_ (logDebug . renderPoseidonException) $ take 5 $ lefts jannoRepresentation
        liftIO $ throwIO $ PoseidonFileConsistencyException jannoPath "Broken lines. See more details with --logMode VerboseLog"
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
    case Csv.decodeByNameWith decodingOptions row of
        Left e -> do
            return $ Left $ PoseidonFileRowException jannoPath lineNumber $ removeUselessSuffix e
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

checkJannoConsistency :: FilePath -> JannoRows -> Either PoseidonException JannoRows
checkJannoConsistency jannoPath xs
    | not $ checkIndividualUnique xs = Left $ PoseidonFileConsistencyException jannoPath
        "The Poseidon_IDs are not unique"
    | otherwise = Right xs

checkIndividualUnique :: JannoRows -> Bool
checkIndividualUnique (JannoRows rows) = length rows == length (nub $ map jPoseidonID rows)

checkJannoRowConsistency :: FilePath -> Int -> JannoRow -> Either PoseidonException JannoRow
checkJannoRowConsistency jannoPath row x
    | not $ checkMandatoryStringNotEmpty x = Left $ PoseidonFileRowException jannoPath row
          "The mandatory columns Poseidon_ID and Group_Name contain empty values"
    | not $ checkC14ColsConsistent x = Left $ PoseidonFileRowException jannoPath row
          "The Date_* columns are not consistent"
    | not $ checkContamColsConsistent x = Left $ PoseidonFileRowException jannoPath row
          "The Contamination_* columns are not consistent"
    | not $ checkRelationColsConsistent x = Left $ PoseidonFileRowException jannoPath row
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
allEqual [] = True
allEqual x  = length (nub x) == 1

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
  let lRelationTo     = getCellLength $ jRelationTo x
      lRelationDegree = getCellLength $ jRelationDegree x
      lRelationType   = getCellLength $ jRelationType x
  in allEqual [lRelationTo, lRelationType, lRelationDegree]
     || (allEqual [lRelationTo, lRelationDegree] && isNothing (jRelationType x))
