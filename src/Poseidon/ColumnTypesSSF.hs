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

