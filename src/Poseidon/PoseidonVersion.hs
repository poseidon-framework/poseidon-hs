module Poseidon.PoseidonVersion (
    currentPoseidonVersion, 
    showPoseidonVersion
) where

import           Data.Version               (Version (..), makeVersion, showVersion)

newtype PoseidonVersion = PoseidonVersion Version
    deriving (Show, Eq, Ord)

currentPoseidonVersion :: PoseidonVersion
currentPoseidonVersion = PoseidonVersion $ makeVersion [2,4,0]

showPoseidonVersion :: PoseidonVersion -> String
showPoseidonVersion (PoseidonVersion x) = showVersion x
