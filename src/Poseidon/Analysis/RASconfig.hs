{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Analysis.RASconfig where

import           Poseidon.Analysis.Utils (GroupDef, parseGroupDefsFromJSON)

import           Data.Aeson              (FromJSON, parseJSON, withObject, (.:),
                                          (.:?))
import           Poseidon.EntitiesList   (EntitiesList, PoseidonEntity (..))

data PopConfig = PopConfigYamlStruct
    { popConfigGroupDef :: [GroupDef]
    , popConfigLefts    :: EntitiesList
    , popConfigRights   :: EntitiesList
    , popConfigOutgroup :: Maybe PoseidonEntity
    }

instance FromJSON PopConfig where
    parseJSON = withObject "PopConfigYamlStruct" $ \v -> PopConfigYamlStruct
        <$> (v .:? "groupDefs" >>= maybe (return []) parseGroupDefsFromJSON)
        <*> v .: "popLefts"
        <*> v .: "popRights"
        <*> v .:? "outgroup"
