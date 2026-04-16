module Poseidon.PoseidonVersion where

import           Data.Aeson   (FromJSON, ToJSON (..), parseJSON, toJSON)
import           Data.Version (Version (..), makeVersion, showVersion)

newtype PoseidonVersion = PoseidonVersion Version
    deriving (Show, Eq, Ord)

instance FromJSON PoseidonVersion where parseJSON v = PoseidonVersion <$> parseJSON v
instance ToJSON PoseidonVersion where toJSON (PoseidonVersion v) = toJSON v

validPoseidonVersions :: [PoseidonVersion]
validPoseidonVersions = map (PoseidonVersion . makeVersion) [[2,5,0], [2,6,0], [2,7,0], [2,7,1], [3,0,0]]

latestPoseidonVersion :: PoseidonVersion
latestPoseidonVersion = last validPoseidonVersions

asVersion :: PoseidonVersion -> Version
asVersion (PoseidonVersion x) = x

showPoseidonVersion :: PoseidonVersion -> String
showPoseidonVersion (PoseidonVersion x) = showVersion x

-- this is for the server
minimalRequiredClientVersion :: Version
minimalRequiredClientVersion = makeVersion [1, 1, 8, 5]

-- and this for validate and jannocoalesce
data VersionedFile = VersionedFile PoseidonVersion FilePath
