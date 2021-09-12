module Poseidon.PoseidonVersion (
    validPoseidonVersions, 
    showPoseidonVersion
) where

import           Data.Version               (Version (..), makeVersion, showVersion)

newtype PoseidonVersion = PoseidonVersion Version
    deriving (Show, Eq, Ord)

validPoseidonVersions :: [PoseidonVersion]
validPoseidonVersions = map (PoseidonVersion . makeVersion) [[2,4,0]]

latestPoseidonVersion :: PoseidonVersion
latestPoseidonVersion = last validPoseidonVersions

showPoseidonVersion :: PoseidonVersion -> String
showPoseidonVersion (PoseidonVersion x) = showVersion x
