module Poseidon.EntitiesList (
    PoseidonEntity (..), SignedEntity (..), 
    SignedEntitiesList, EntitiesList, 
    readEntitiesFromFile, readPoseidonEntitiesString,
    entityIncludes, entityExcludes
    ) where

import           Poseidon.Utils             (PoseidonException(..))

import           Control.Applicative        ((<|>))
import           Control.Exception          (throwIO)
import           Control.Monad              (ap)
import           Data.Char                  (isSpace)
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

-- | A datatype to represent a package, a group or an individual
data PoseidonEntity =
      Pac String
    | Group String
    | Ind String
    deriving (Eq)

instance Show PoseidonEntity where
    show (Pac   n) = "*" ++ n ++ "*"
    show (Group n) = n
    show (Ind   n) = "<" ++ n ++ ">"

type EntitiesList = [PoseidonEntity]

data SignedEntity = 
      Include PoseidonEntity 
    | Exclude PoseidonEntity
    deriving (Eq)

instance Show SignedEntity where
    show (Include a) = show a
    show (Exclude a) = "-" ++ show a

type SignedEntitiesList = [SignedEntity]

isInclude :: SignedEntity -> Bool
isInclude (Include _) = True
isInclude _ = False

isExclude :: SignedEntity -> Bool
isExclude (Exclude _) = True
isExclude _ = False

fromSignedEntity :: SignedEntity -> PoseidonEntity
fromSignedEntity (Include a) = a
fromSignedEntity (Exclude a) = a

entityIncludes :: SignedEntitiesList -> EntitiesList
entityIncludes xs = map fromSignedEntity $ filter isInclude xs

entityExcludes :: SignedEntitiesList -> EntitiesList
entityExcludes xs = map fromSignedEntity $ filter isExclude xs

-- | A parser to parse entities
parseSignedEntitiesList :: P.Parser SignedEntitiesList
parseSignedEntitiesList = do 
    eL <- P.sepBy parsePoseidonEntity (P.char ',' <* P.spaces)
    P.optional (P.many (P.char ' ') >> comment)
    return eL

parseSignedEntitiesListFromFile :: P.Parser SignedEntitiesList
parseSignedEntitiesListFromFile = do
    concat <$> P.sepBy (P.try emptyLine <|> comment <|> parseSignedEntitiesList) P.newline

comment :: P.Parser SignedEntitiesList
comment = do
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar (P.lookAhead P.newline)
    return []

emptyLine :: P.Parser SignedEntitiesList
emptyLine = do
    _ <- P.manyTill (P.char ' ') (P.lookAhead P.newline)
    return []

parsePoseidonEntity :: P.Parser SignedEntity
parsePoseidonEntity = ap parseSign (parsePac <|> parseGroup <|> parseInd)
  where
    parseSign = (P.char '-' >> return Exclude) <|> (P.optional (P.char '+') >> return Include)
    parsePac   = Pac   <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = Group <$> parseName
    parseInd   = Ind   <$> P.between (P.char '<') (P.char '>') parseName
    parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

readEntitiesFromFile :: FilePath -> IO SignedEntitiesList
readEntitiesFromFile entitiesFile = do
    eitherParseResult <- P.parseFromFile (parseSignedEntitiesListFromFile <* P.eof) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonPoseidonEntityParsingException (show err))
        Right r -> return r

readPoseidonEntitiesString :: String -> Either String SignedEntitiesList
readPoseidonEntitiesString s = case P.runParser (parseSignedEntitiesList <* P.eof) () "" s of
    Left p  -> Left (show p)
    Right x -> Right x
