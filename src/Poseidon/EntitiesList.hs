module Poseidon.EntitiesList (
    PoseidonEntity (..), SignedEntity (..),
    SignedEntitiesList, EntitiesList, EntitySpec,
    readEntitiesFromFile, readEntitiesFromString,
    findNonExistentEntities, indInfoFindRelevantPackageNames, filterRelevantPackages,
    conformingEntityIndices) where

import           Poseidon.Package        (PoseidonPackage (..),
                                          getJointIndividualInfo)
import           Poseidon.SecondaryTypes (IndividualInfo (..))
import           Poseidon.Utils          (PoseidonException (..))

import           Control.Applicative     ((<|>))
import           Control.Exception       (throwIO)
import           Data.Char               (isSpace)
import           Data.List               (nub, (\\))
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P

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

data SignedEntity = Include PoseidonEntity
    | Exclude PoseidonEntity
    deriving (Eq)

instance Show SignedEntity where
    show (Include a) = show a
    show (Exclude a) = "-" ++ show a

type SignedEntitiesList = [SignedEntity]

-- A class to generalise signed and unsigned Entity Lists. Both have the feature that they can be used to filter individuals.
class EntitySpec a where
    indInfoConformsToEntitySpec :: [a] -> IndividualInfo -> Bool
    underlyingEntity :: a -> PoseidonEntity
    entitySpecParser :: P.Parser a

instance EntitySpec SignedEntity where
    indInfoConformsToEntitySpec signedEntities (IndividualInfo indName groupNames pacName) = go signedEntities False
      where
        go [] r = r
        go (Include (Ind   n):rest) r = if n ==     indName    then go rest True  else go rest r
        go (Include (Group n):rest) r = if n `elem` groupNames then go rest True  else go rest r
        go (Include (Pac   n):rest) r = if n ==     pacName    then go rest True  else go rest r
        go (Exclude (Ind   n):rest) r = if n ==     indName    then go rest False else go rest r
        go (Exclude (Group n):rest) r = if n `elem` groupNames then go rest False else go rest r
        go (Exclude (Pac   n):rest) r = if n ==     pacName    then go rest False else go rest r
    underlyingEntity = removeEntitySign
    entitySpecParser = parseSign <*> entitySpecParser
      where
        parseSign = (P.char '-' >> return Exclude) <|> (P.optional (P.char '+') >> return Include)

instance EntitySpec PoseidonEntity where
    indInfoConformsToEntitySpec entities = indInfoConformsToEntitySpec (map Include entities)
    underlyingEntity = id
    entitySpecParser = parsePac <|> parseGroup <|> parseInd
      where
        parsePac   = Pac   <$> P.between (P.char '*') (P.char '*') parseName
        parseGroup = Group <$> parseName
        parseInd   = Ind   <$> P.between (P.char '<') (P.char '>') parseName
        parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

removeEntitySign :: SignedEntity -> PoseidonEntity
removeEntitySign (Include e) = e
removeEntitySign (Exclude e) = e

entitiesListP :: (EntitySpec a) => P.Parser [a]
entitiesListP = P.sepBy entitySpecParser (P.char ',' <* P.spaces)

entitiesListMultilineP :: (EntitySpec a) => P.Parser [a]
entitiesListMultilineP = do
    concat <$> P.sepBy (P.try emptyLineP <|> commentP <|> entitiesListWithCommentsP) P.newline
  where
    entitiesListWithCommentsP = do
        eL <- entitiesListP
        _ <- P.many (P.char ' ')
        _ <- P.optional (commentP :: P.Parser EntitiesList) -- compiler complains about unambiguous type without this redundant type annotation
        return eL

emptyLineP :: (EntitySpec a) => P.Parser [a]
emptyLineP = do
    _ <- P.manyTill (P.char ' ') (P.lookAhead P.newline)
    return []

commentP :: (EntitySpec a) => P.Parser [a]
commentP = do
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar (P.lookAhead P.newline)
    return []

readEntitiesFromFile :: (EntitySpec a) => FilePath -> IO [a]
readEntitiesFromFile entitiesFile = do
    eitherParseResult <- P.parseFromFile (entitiesListMultilineP <* P.eof) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonPoseidonEntityParsingException (show err))
        Right r -> return r

readEntitiesFromString :: (EntitySpec a) => String -> Either PoseidonException [a]
readEntitiesFromString s = case P.runParser (entitiesListP <* P.eof) () "" s of
    Left p  -> Left $ PoseidonPoseidonEntityParsingException (show p)
    Right x -> Right x

indInfoFindRelevantPackageNames :: (EntitySpec a) => [a] -> [IndividualInfo] -> [String]
indInfoFindRelevantPackageNames e =
    nub . map indInfoPacName . filter (indInfoConformsToEntitySpec e)

filterRelevantPackages :: (EntitySpec a) => [a] -> [PoseidonPackage] -> [PoseidonPackage]
filterRelevantPackages e packages =
    let pacNames = indInfoFindRelevantPackageNames e (getJointIndividualInfo packages)
    in  filter ((`elem` pacNames) . posPacTitle) packages

findNonExistentEntities :: (EntitySpec a) => [a] -> [IndividualInfo] -> EntitiesList
findNonExistentEntities entities individuals =
    let titlesPac     = nub . map indInfoPacName $ individuals
        indNamesPac   = map indInfoName individuals
        groupNamesPac = nub . concatMap indInfoGroups $ individuals
        titlesRequestedPacs = [ pac   | Pac   pac   <- map underlyingEntity entities]
        groupNamesStats     = [ group | Group group <- map underlyingEntity entities]
        indNamesStats       = [ ind   | Ind   ind   <- map underlyingEntity entities]
        missingPacs   = map Pac   $ titlesRequestedPacs \\ titlesPac
        missingInds   = map Ind   $ indNamesStats       \\ indNamesPac
        missingGroups = map Group $ groupNamesStats     \\ groupNamesPac
    in  missingPacs ++ missingInds ++ missingGroups

conformingEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> [Int]
conformingEntityIndices entities = map fst . filter (indInfoConformsToEntitySpec entities .  snd) . zip [0..]
