module Poseidon.MetaData (PoseidonMetaData) where

import Data.Map (Map)

type PoseidonMetaData = Map String MetaDataEntry

type GeoPosition = (Double, Double)

data Age = undefined

data MetaDataEntry = MetaDataEntry {
    mdCountry :: String,
    mdGeoPosition :: GeoPosition,
    mdAge :: Age
}