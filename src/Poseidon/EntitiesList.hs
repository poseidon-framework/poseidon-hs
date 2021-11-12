module Poseidon.EntitiesList (
    PoseidonEntity (..), EntitiesList, 
    readEntitiesFromFile, readPoseidonEntitiesString
    ) where

import           Poseidon.Utils             (PoseidonException(..))

import           Control.Applicative        ((<|>))
import           Control.Exception          (throwIO)
import           Data.Char                  (isSpace)
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P
import Control.Monad (when)
import Text.Parsec.Prim (lookAhead)

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
parsePoseidonEntitiesList :: P.Parser EntitiesList
parsePoseidonEntitiesList = do 
    eL <- P.sepBy parsePoseidonEntity (P.char ',' <* P.spaces)
    P.optional (P.many (P.char ' ') >> comment)
    return eL

parsePoseidonEntitiesListFromFile :: P.Parser EntitiesList
parsePoseidonEntitiesListFromFile = do
    concat <$> P.sepBy (P.try emptyLine <|> comment <|> parsePoseidonEntitiesList) P.newline

comment :: P.Parser EntitiesList
comment = do
    P.string "#"
    P.manyTill P.anyChar (P.lookAhead P.newline)
    return []

emptyLine :: P.Parser EntitiesList
emptyLine = do
    P.manyTill (P.char ' ') (P.lookAhead P.newline)
    return []

parsePoseidonEntity :: P.Parser PoseidonEntity
parsePoseidonEntity = parsePac <|> parseGroup <|> parseInd
  where
    parsePac   = Pac   <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = Group <$> parseName
    parseInd   = Ind   <$> P.between (P.char '<') (P.char '>') parseName
    parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

readEntitiesFromFile :: FilePath -> IO EntitiesList
readEntitiesFromFile entitiesFile = do
    eitherParseResult <- P.parseFromFile (parsePoseidonEntitiesListFromFile <* P.eof) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonPoseidonEntityParsingException (show err))
        Right r -> return r

readPoseidonEntitiesString :: String -> Either String [PoseidonEntity]
readPoseidonEntitiesString s = case P.runParser (parsePoseidonEntitiesList <* P.eof) () "" s of
    Left p  -> Left (show p)
    Right x -> Right x
