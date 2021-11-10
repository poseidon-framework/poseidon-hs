module Poseidon.EntitiesList (
    PoseidonEntity (..), EntitiesList, 
    poseidonEntitiesParser, readEntitiesFromFile, readPoseidonEntitiesString
    ) where

import           Poseidon.Utils             (PoseidonException(..))

import           Control.Applicative        ((<|>))
import           Control.Exception          (throwIO)
import           Data.Char                  (isSpace)
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

-- | A datatype to represent a package, a group or an individual
data PoseidonEntity = Pac String
    | Group String
    | Ind String
    deriving (Eq)

instance Show PoseidonEntity where
    show (Pac   n) = "*" ++ n ++ "*"
    show (Group n) = n
    show (Ind   n) = "<" ++ n ++ ">"

type EntitiesList = [PoseidonEntity]

-- | A parser to parse entities
poseidonEntitiesParser :: P.Parser EntitiesList
poseidonEntitiesParser = P.try (P.sepBy parsePoseidonEntity (P.char ',' <* P.spaces))

parsePoseidonEntity :: P.Parser PoseidonEntity
parsePoseidonEntity = parsePac <|> parseGroup <|> parseInd
  where
    parsePac   = Pac   <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = Group <$> parseName
    parseInd   = Ind   <$> P.between (P.char '<') (P.char '>') parseName
    parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

readEntitiesFromFile :: FilePath -> IO EntitiesList
readEntitiesFromFile entitiesFile = do
    let multiEntityParser = poseidonEntitiesParser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- P.parseFromFile (P.spaces *> multiEntityParser <* P.spaces) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonPoseidonEntityParsingException (show err))
        Right r -> return (concat r)

readPoseidonEntitiesString :: String -> Either String [PoseidonEntity]
readPoseidonEntitiesString s = case P.runParser poseidonEntitiesParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x
