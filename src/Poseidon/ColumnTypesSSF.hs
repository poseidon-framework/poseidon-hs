{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Poseidon.ColumnTypesSSF where

import           Poseidon.ColumnTypesUtils

import           Country                    (Country, alphaTwoUpper,
                                             decodeAlphaTwo)
import qualified Data.Csv                   as Csv
import qualified Data.Text                  as T
import qualified Data.Text.Read             as T
import           GHC.Generics               (Generic)
import           Network.URI                (isURIReference)
import           SequenceFormats.Eigenstrat (Sex (..))
import qualified Text.Regex.TDFA            as Reg

-- | A datatype for the Genetic_Sex .janno column

-- |A datatype to represent AccessionIDs in a ssf file
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