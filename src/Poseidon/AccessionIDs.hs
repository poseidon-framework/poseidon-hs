{-# LANGUAGE DeriveGeneric     #-}

module Poseidon.AccessionIDs where

import           GHC.Generics              (Generic)
import qualified Text.Regex.TDFA           as Reg
import qualified Data.Text                 as T

-- | A datatype to represent AccessionIDs in a ssf file
data AccessionID =
      INSDCProject T.Text
    | INSDCStudy T.Text
    | INSDCBioSample T.Text
    | INSDCSample T.Text
    | INSDCExperiment T.Text
    | INSDCRun T.Text
    | INSDCAnalysis T.Text
    | OtherID T.Text
    deriving (Eq, Ord, Generic)

instance Show AccessionID where
    show (INSDCProject x)    = T.unpack x
    show (INSDCStudy x)      = T.unpack x
    show (INSDCBioSample x)  = T.unpack x
    show (INSDCSample x)     = T.unpack x
    show (INSDCExperiment x) = T.unpack x
    show (INSDCRun x)        = T.unpack x
    show (INSDCAnalysis x)   = T.unpack x
    show (OtherID x)         = T.unpack x

-- the patterns are documented at:
-- https://ena-docs.readthedocs.io/en/latest/submit/general-guide/accessions.html
makeAccessionID :: MonadFail m => T.Text -> m AccessionID
makeAccessionID x
    | (T.unpack x) Reg.=~ ("PRJ[EDN][A-Z][0-9]+"  :: String) = pure $ INSDCProject x
    | (T.unpack x) Reg.=~ ("[EDS]RP[0-9]{6,}"     :: String) = pure $ INSDCStudy x
    | (T.unpack x) Reg.=~ ("SAM[EDN][A-Z]?[0-9]+" :: String) = pure $ INSDCBioSample x
    | (T.unpack x) Reg.=~ ("[EDS]RS[0-9]{6,}"     :: String) = pure $ INSDCSample x
    | (T.unpack x) Reg.=~ ("[EDS]RX[0-9]{6,}"     :: String) = pure $ INSDCExperiment x
    | (T.unpack x) Reg.=~ ("[EDS]RR[0-9]{6,}"     :: String) = pure $ INSDCRun x
    | (T.unpack x) Reg.=~ ("[EDS]RZ[0-9]{6,}"     :: String) = pure $ INSDCAnalysis x
    | otherwise                                   = pure $ OtherID x
