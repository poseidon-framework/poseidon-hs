{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Poseidon.ColumnTypesSSF where

import           Poseidon.AccessionIDs
import           Poseidon.ColumnTypesUtils
import Poseidon.Utils (logWarning)

import           Data.Char                 (isHexDigit)
import qualified Data.Csv                  as Csv
import qualified Data.Text                 as T
import qualified Data.Text.Read            as T
import           Data.Time                 (Day)
import           Data.Time.Format          (defaultTimeLocale, formatTime,
                                            parseTimeM)
import           GHC.Generics              (Generic)
import           Network.URI               (isURIReference)

-- | A datatype for the udg .ssf column
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
instance Suspicious SSFUDG where inspect _ = pure ()
instance Show SSFUDG where
    show SSFMinus = "minus"
    show SSFHalf  = "half"
    show SSFPlus  = "plus"
instance Csv.ToField SSFUDG where   toField x = Csv.toField $ show x
instance Csv.FromField SSFUDG where parseField = parseTypeCSV "udg"

-- | A datatype for the library_built .ssf column
data SSFLibraryBuilt =
      SSFDS
    | SSFSS
    deriving (Eq, Ord, Generic, Enum, Bounded)

instance Makeable SSFLibraryBuilt where
    make x
        | x == "ds"    = pure SSFDS
        | x == "ss"    = pure SSFSS
        | otherwise    = fail $ "library_built is set to " ++ show x ++ ". " ++
                                "That is not in [ds, ss]."
instance Suspicious SSFLibraryBuilt where inspect _ = pure ()
instance Show SSFLibraryBuilt where
    show SSFDS = "ds"
    show SSFSS = "ss"
instance Csv.ToField SSFLibraryBuilt where   toField x = Csv.toField $ show x
instance Csv.FromField SSFLibraryBuilt where parseField = parseTypeCSV "library_built"

-- | A datatype for the sample_accession .ssf column
newtype SSFAccessionIDSample = SSFAccessionIDSample AccessionID
    deriving (Eq, Ord, Generic)

instance Makeable SSFAccessionIDSample where
    make x = do
        accID <- makeAccessionID x
        case accID of
            i@(INSDCBioSample _) -> return $ SSFAccessionIDSample i
            i@(INSDCSample _) -> return $ SSFAccessionIDSample i
            --i@(OtherID _) -> return $ SSFAccessionIDSample i
            i -> fail $ "sample_accession " ++ show i ++ " is not a correct biosample/sample accession."
instance Suspicious SSFAccessionIDSample where inspect _ = pure ()
instance Show SSFAccessionIDSample where
    show (SSFAccessionIDSample x) = show x
instance Csv.ToField SSFAccessionIDSample where   toField x  = Csv.toField $ show x
instance Csv.FromField SSFAccessionIDSample where parseField = parseTypeCSV "sample_accession"

-- | A datatype for the study_accession .ssf column
newtype SSFAccessionIDStudy = SSFAccessionIDStudy AccessionID
    deriving (Eq, Ord, Generic)

instance Makeable SSFAccessionIDStudy where
    make x = do
        accID <- makeAccessionID x
        case accID of
            i@(INSDCProject _) -> return $ SSFAccessionIDStudy i
            i@(INSDCStudy _) -> return $ SSFAccessionIDStudy i
            --i@(OtherID _) -> return $ SSFAccessionIDStudy i
            i -> fail $ "study_accession " ++ show i ++ " is not a correct project/study accession."
instance Suspicious SSFAccessionIDStudy where inspect _ = pure ()
instance Show SSFAccessionIDStudy where
    show (SSFAccessionIDStudy x) = show x
instance Csv.ToField SSFAccessionIDStudy where   toField x  = Csv.toField $ show x
instance Csv.FromField SSFAccessionIDStudy where parseField = parseTypeCSV "study_accession"

-- | A datatype for the run_accession .ssf column
newtype SSFAccessionIDRun = SSFAccessionIDRun AccessionID
    deriving (Eq, Ord, Generic)

instance Makeable SSFAccessionIDRun where
    make x = do
        accID <- makeAccessionID x
        case accID of
            i@(INSDCRun _) -> return $ SSFAccessionIDRun i
            --i@(OtherID _) -> return $ SSFAccessionIDRun i
            i -> fail $ "run_accession " ++ show i ++ " is not a correct run accession."
instance Suspicious SSFAccessionIDRun where inspect _ = pure ()
instance Show SSFAccessionIDRun where
    show (SSFAccessionIDRun x) = show x
instance Csv.ToField SSFAccessionIDRun where   toField x  = Csv.toField $ show x
instance Csv.FromField SSFAccessionIDRun where parseField = parseTypeCSV "run_accession"

-- | A datatype for the sample_alias .ssf column
newtype SSFSampleAlias = SSFSampleAlias T.Text deriving (Eq, Ord)
$(makeInstances ''SSFSampleAlias "sample_alias")

-- | A datatype for the secondary_sample_accession .ssf column
newtype SSFSecondarySampleAccession = SSFSecondarySampleAccession T.Text deriving (Eq, Ord)
$(makeInstances ''SSFSecondarySampleAccession "secondary_sample_accession")

-- | A datatype for the first_public .ssf column
newtype SSFFirstPublicSimpleDate = SSFFirstPublicSimpleDate Day
    deriving (Eq, Ord, Generic)

instance Makeable SSFFirstPublicSimpleDate where
    make x = do
          case parseTimeM False defaultTimeLocale "%Y-%-m-%-d" (T.unpack x) :: Maybe Day of
            Nothing -> fail $ "first_public date " ++ T.unpack x ++
                              " is not a correct date in the format YYYY-MM-DD."
            Just d  -> pure (SSFFirstPublicSimpleDate d)
instance Suspicious SSFFirstPublicSimpleDate where inspect _ = pure ()
instance Show SSFFirstPublicSimpleDate where
    show (SSFFirstPublicSimpleDate x) = formatTime defaultTimeLocale "%Y-%-m-%-d" x
instance Csv.ToField SSFFirstPublicSimpleDate where
    toField (SSFFirstPublicSimpleDate x) = Csv.toField $ show x
instance Csv.FromField SSFFirstPublicSimpleDate where
    parseField = parseTypeCSV "first_public"

-- | A datatype for the last_updated .ssf column
newtype SSFLastUpdatedSimpleDate = SSFLastUpdatedSimpleDate Day
    deriving (Eq, Ord, Generic)

instance Makeable SSFLastUpdatedSimpleDate where
    make x = do
          case parseTimeM False defaultTimeLocale "%Y-%-m-%-d" (T.unpack x) :: Maybe Day of
            Nothing -> fail $ "last_updated date " ++ T.unpack x ++
                              " is not a correct date in the format YYYY-MM-DD."
            Just d  -> pure (SSFLastUpdatedSimpleDate d)
instance Suspicious SSFLastUpdatedSimpleDate where inspect _ = pure ()
instance Show SSFLastUpdatedSimpleDate where
    show (SSFLastUpdatedSimpleDate x) = formatTime defaultTimeLocale "%Y-%-m-%-d" x
instance Csv.ToField SSFLastUpdatedSimpleDate where
    toField (SSFLastUpdatedSimpleDate x) = Csv.toField $ show x
instance Csv.FromField SSFLastUpdatedSimpleDate where
    parseField = parseTypeCSV "last_updated"

-- | A datatype for the instrument_model .ssf column
newtype SSFInstrumentModel = SSFInstrumentModel T.Text deriving (Eq, Ord)
$(makeInstances ''SSFInstrumentModel "instrument_model")

-- | A datatype for the library_layout .ssf column
newtype SSFLibraryLayout = SSFLibraryLayout T.Text deriving (Eq, Ord)
$(makeInstances ''SSFLibraryLayout "library_layout")

-- | A datatype for the library_source .ssf column
newtype SSFLibrarySource = SSFLibrarySource T.Text deriving (Eq, Ord)
$(makeInstances ''SSFLibrarySource "library_source")

-- | A datatype for the instrument_platform .ssf column
newtype SSFInstrumentPlatform = SSFInstrumentPlatform T.Text deriving (Eq, Ord)
$(makeInstances ''SSFInstrumentPlatform "instrument_platform")

-- | A datatype for the library_name .ssf column
newtype SSFLibraryName = SSFLibraryName T.Text deriving (Eq, Ord)
$(makeInstances ''SSFLibraryName "library_name")

-- | A datatype for the library_strategy .ssf column
newtype SSFLibraryStrategy = SSFLibraryStrategy T.Text deriving (Eq, Ord)
$(makeInstances ''SSFLibraryStrategy "library_strategy")

-- | A datatype for the fastq_ftp .ssf column
newtype SSFFastqFTPURI = SSFFastqFTPURI T.Text
    deriving (Eq, Ord, Generic)

instance Makeable SSFFastqFTPURI where
    make x
        | isURIReference (T.unpack x) = pure $ SSFFastqFTPURI x
        | otherwise                   = fail $ "fastq_ftp entry " ++ show x ++
                                               " is not a well-structured URI."
instance Suspicious SSFFastqFTPURI where inspect _ = pure ()
instance Show SSFFastqFTPURI where show (SSFFastqFTPURI x) = T.unpack x
instance Csv.ToField SSFFastqFTPURI where toField x = Csv.toField $ show x
instance Csv.FromField SSFFastqFTPURI where parseField = parseTypeCSV "fastq_ftp"

-- | A datatype for the fastq_aspera .ssf column
newtype SSFFastqASPERAURI = SSFFastqASPERAURI T.Text
    deriving (Eq, Ord, Generic)

instance Makeable SSFFastqASPERAURI where
    make x
        | isURIReference (T.unpack x) = pure $ SSFFastqASPERAURI x
        | otherwise                   = fail $ "fastq_aspera entry " ++ show x ++
                                               " is not a well-structured URI."
instance Suspicious SSFFastqASPERAURI where inspect _ = pure ()
instance Show SSFFastqASPERAURI where show (SSFFastqASPERAURI x) = T.unpack x
instance Csv.ToField SSFFastqASPERAURI where toField x = Csv.toField $ show x
instance Csv.FromField SSFFastqASPERAURI where parseField = parseTypeCSV "fastq_aspera"

-- | A datatype for the fastq_bytes .ssf column
newtype SSFFastqBytes = SSFFastqBytes Integer deriving (Eq, Ord, Generic)

instance Makeable SSFFastqBytes where
    make x =
        case T.decimal x of
            Left e -> fail $ "fastq_bytes can not be converted to Integer because " ++ e
            Right (num, "") -> pure $ SSFFastqBytes num
            Right (_, rest) -> fail $ "fastq_bytes can not be converted to Integer, because of a trailing " ++ show rest
instance Suspicious SSFFastqBytes where inspect _ = pure ()
instance Show SSFFastqBytes where          show (SSFFastqBytes x) = show x
instance Csv.ToField SSFFastqBytes where   toField (SSFFastqBytes x) = Csv.toField x
instance Csv.FromField SSFFastqBytes where parseField = parseTypeCSV "fastq_bytes"

-- | A datatype for the fastq_md5 .ssf column
newtype SSFFastqMD5 = SSFFastqMD5 T.Text deriving (Eq, Ord, Generic)

instance Makeable SSFFastqMD5 where
    make x
        | isMD5Hash x = pure $ SSFFastqMD5 x
        | otherwise   = fail $ "fastq_md5 " ++ show x ++
                               " does not contain a well-structured MD5 hash"
isMD5Hash :: T.Text -> Bool
isMD5Hash x = T.length x == 32 && T.all isHexDigit x
instance Suspicious SSFFastqMD5 where inspect _ = pure ()
instance Show SSFFastqMD5 where show (SSFFastqMD5 x) = T.unpack x
instance Csv.ToField SSFFastqMD5 where   toField x = Csv.toField $ show x
instance Csv.FromField SSFFastqMD5 where parseField = parseTypeCSV "fastq_md5"

-- | A datatype for the fastq_bytes .ssf column
newtype SSFReadCount = SSFReadCount Integer deriving (Eq, Ord, Generic)

instance Makeable SSFReadCount where
    make x =
        case (T.signed T.decimal) x of -- the ENA uses -1 in case the read count failed
            Left e -> fail $ "read_count can not be converted to Integer because " ++ e
            Right (num, "") -> pure $ SSFReadCount num
            Right (_, rest) -> fail $ "read_count can not be converted to Integer, because of a trailing " ++ show rest
instance Suspicious SSFReadCount where
    inspect (SSFReadCount x) | x < 0 = logWarning "Fishy! Fishy! Fishy!"
                             | otherwise = pure ()
instance Show SSFReadCount where          show (SSFReadCount x) = show x
instance Csv.ToField SSFReadCount where   toField (SSFReadCount x) = Csv.toField x
instance Csv.FromField SSFReadCount where parseField = parseTypeCSV "read_count"

-- | A datatype for the submitted_ftp .ssf column
newtype SSFSubmittedFTPURI = SSFSubmittedFTPURI T.Text
    deriving (Eq, Ord, Generic)

instance Makeable SSFSubmittedFTPURI where
    make x
        | isURIReference (T.unpack x) = pure $ SSFSubmittedFTPURI x
        | otherwise                   = fail $ "submitted_ftp entry " ++ show x ++
                                               " is not a well-structured URI."
instance Suspicious SSFSubmittedFTPURI where inspect _ = pure ()
instance Show SSFSubmittedFTPURI where show (SSFSubmittedFTPURI x) = T.unpack x
instance Csv.ToField SSFSubmittedFTPURI where toField x = Csv.toField $ show x
instance Csv.FromField SSFSubmittedFTPURI where parseField = parseTypeCSV "submitted_ftp"

