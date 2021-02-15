module Poseidon.EntitiesList (
    PoseidonEntity (..), EntitiesList (..), 
    forgeEntitiesParser, readEntitiesFromFile
    ) where

import           Poseidon.Utils             (PoseidonException(..))

import           Control.Applicative        ((<|>))
import           Control.Exception          (throwIO)
import           Data.Char                  (isSpace)
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

-- | A datatype to represent a package, a group or an individual
data PoseidonEntity = ForgePac String
    | ForgeGroup String
    | ForgeInd String
    deriving (Eq)

instance Show PoseidonEntity where
    show (ForgePac   n) = "*" ++ n ++ "*"
    show (ForgeGroup n) = n
    show (ForgeInd   n) = "<" ++ n ++ ">"

type EntitiesList = [PoseidonEntity]

-- | A parser to parse forge entities
forgeEntitiesParser :: P.Parser EntitiesList
forgeEntitiesParser = P.try (P.sepBy parsePoseidonEntity (P.char ',' <* P.spaces))

parsePoseidonEntity :: P.Parser PoseidonEntity
parsePoseidonEntity = parsePac <|> parseGroup <|> parseInd
  where
    parsePac   = ForgePac   <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = ForgeGroup <$> parseName
    parseInd   = ForgeInd   <$> P.between (P.char '<') (P.char '>') parseName
    parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

readEntitiesFromFile :: FilePath -> IO EntitiesList
readEntitiesFromFile entitiesFile = do
    let multiEntityParser = forgeEntitiesParser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- P.parseFromFile (P.spaces *> multiEntityParser <* P.spaces) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonPoseidonEntityParsingException (show err))
        Right r -> return (concat r)