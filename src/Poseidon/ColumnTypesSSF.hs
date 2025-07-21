{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Poseidon.ColumnTypesSSF where

import           Poseidon.ColumnTypesUtils

import           Data.Char                 (isHexDigit)
import qualified Data.Csv                  as Csv
import           Data.Time                 (Day)
import           Data.Time.Format          (defaultTimeLocale, formatTime,
                                            parseTimeM)
import           GHC.Generics              (Generic)
import           Network.URI               (isURIReference)
import qualified Text.Regex.TDFA           as Reg

-- |A datatype to represent UDG in a ssf file
data SSFUDG =
      SSFMinus
    | SSFHalf
    | SSFPlus
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable SSFUDG where
    make x
        | x == "minus" = pure SSFMinus
        | x == "half"  = pure SSFHalf
        | x == "plus"  = pure SSFPlus
        | otherwise    = fail $ "udg is set to " ++ show x ++ ". " ++
                                "That is not in the allowed set [minus, half, plus]."
instance Show SSFUDG where
    show SSFMinus = "minus"
    show SSFHalf  = "half"
    show SSFPlus  = "plus"
instance Csv.ToField SSFUDG where   toField x = Csv.toField $ show x
instance Csv.FromField SSFUDG where parseField = parseTypeCSV "udg"

-- | A datatype to represent AccessionIDs in a ssf file
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

-- | A datatype to represent URIs in a ssf file
newtype JURI =
        JURI String
    deriving (Eq, Ord, Generic)

instance Show JURI where
    show (JURI x) = x

makeJURI :: MonadFail m => String -> m JURI
makeJURI x
    | isURIReference x   = pure $ JURI x
    | otherwise          = fail $ "URI " ++ show x ++ " not well structured"

instance Csv.ToField JURI where
    toField x = Csv.toField $ show x
instance Csv.FromField JURI where
    parseField x = Csv.parseField x >>= makeJURI

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
