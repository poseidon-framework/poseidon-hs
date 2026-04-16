module Poseidon.Version (
    VersionComponent (..),
    updateThreeComponentVersion,
    parseVersion
) where

import           Data.Version       (Version (..), makeVersion)
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P


data VersionComponent = Major
    | Minor
    | Patch
    deriving Show

updateThreeComponentVersion :: VersionComponent -> Version -> Version
updateThreeComponentVersion component v =
    let i = versionBranch v
        r = case component of
            Patch -> [ i !! 0,      i !! 1,     (i !! 2) + 1]
            Minor -> [ i !! 0,     (i !! 1) + 1, 0          ]
            Major -> [(i !! 0) + 1,              0, 0       ]
    in makeVersion r

parseVersion :: P.Parser Version
parseVersion = do
    major <- read <$> P.many1 P.digit
    _ <- P.oneOf "."
    minor <- read <$> P.many1 P.digit
    _ <- P.oneOf "."
    patch <- read <$> P.many1 P.digit
    return (makeVersion [major, minor, patch])
