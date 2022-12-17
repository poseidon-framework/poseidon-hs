module Poseidon.EntitiesList (
    PoseidonEntity (..), SignedEntity (..),
    SignedEntitiesList, EntitiesList, EntitySpec,
    indInfoConformsToEntitySpec, underlyingEntity, entitySpecParser,
    readEntitiesFromFile, readEntitiesFromString,
    findNonExistentEntities, indInfoFindRelevantPackageNames, filterRelevantPackages,
    conformingEntityIndices, entitiesListP, EntityInput(..), readEntityInputs, getIndName, PoseidonIndividual (..)) where

import           Poseidon.Package        (PoseidonPackage (..),
                                          getJointIndividualInfo)
import           Poseidon.SecondaryTypes (IndividualInfo (..))
import           Poseidon.Utils          (PoseidonException (..))

import           Control.Applicative     ((<|>))
import           Control.Exception       (throwIO)
import           Control.Monad           (forM)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON (..), ToJSON (..),
                                          Value (..), withText)
import           Data.Aeson.Types        (Parser)
import           Data.Char               (isSpace)
import           Data.Function           ((&))
import           Data.List               (nub, (\\))
import           Data.Maybe              (mapMaybe)
import           Data.Text               (Text, pack, unpack)
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P

-- | A datatype to represent a package, a group or an individual
data PoseidonEntity =
      Pac String
    | Group String
    | Ind PoseidonIndividual
    deriving (Eq, Ord)

data PoseidonIndividual =
      SimpleInd String
    | SpecificInd IndividualInfo
    deriving (Eq, Ord)

getIndName :: PoseidonIndividual -> String
getIndName (SimpleInd n)       = n
getIndName (SpecificInd (IndividualInfo n _ _)) = n

instance Show PoseidonIndividual where
    show (SimpleInd                   i     ) = "<" ++ i ++ ">"
    show (SpecificInd (IndividualInfo i g p)) = "<" ++ p ++ ":" ++ (head g) ++ ":" ++ i ++ ">"

instance Show PoseidonEntity where
    show (Pac   p) = "*" ++ p ++ "*"
    show (Group g) = g
    show (Ind   i) = show i

type EntitiesList = [PoseidonEntity]

data SignedEntity =
      Include PoseidonEntity
    | Exclude PoseidonEntity
    deriving (Eq, Ord)

instance Show SignedEntity where
    show (Include a) = show a
    show (Exclude a) = "-" ++ show a

type SignedEntitiesList = [SignedEntity]

-- A class to generalise signed and unsigned Entity Lists. Both have the feature that they can be used to filter individuals.
class Eq a => EntitySpec a where
    indInfoConformsToEntitySpec :: [a] -> IndividualInfo -> Bool
    underlyingEntity :: a -> PoseidonEntity
    entitySpecParser :: P.Parser a

instance EntitySpec SignedEntity where
    indInfoConformsToEntitySpec signedEntities indInfo@(IndividualInfo indName groupNames pacName) =
      case mapMaybe shouldIncExc signedEntities of
          [] -> False
          xs -> last xs
      where
        shouldIncExc :: SignedEntity -> Maybe Bool
        shouldIncExc (Include entity) = if entity & isIndInfo then Just True  else Nothing
        shouldIncExc (Exclude entity) = if entity & isIndInfo then Just False else Nothing
        isIndInfo :: PoseidonEntity -> Bool
        isIndInfo (Ind (SimpleInd n)) = n == indName
        isIndInfo (Ind (SpecificInd i)) = i == indInfo
        isIndInfo (Group n) = n `elem` groupNames
        isIndInfo (Pac   n) = n == pacName
    underlyingEntity = removeEntitySign
    entitySpecParser = parseSign <*> entitySpecParser
      where
        parseSign = (P.char '-' >> return Exclude) <|> (P.optional (P.char '+') >> return Include)


instance EntitySpec PoseidonEntity where
    indInfoConformsToEntitySpec entities = indInfoConformsToEntitySpec (map Include entities)
    underlyingEntity = id
    entitySpecParser = parsePac <|> parseGroup <|> parseInd
      where
        parsePac         = Pac   <$> P.between (P.char '*') (P.char '*') parseName
        parseGroup       = Group <$> parseName
        parseInd         = Ind   <$> (P.try parseSimpleInd <|> parseSpecificInd)
        parseName        = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ":,<>*")))
        parseSimpleInd   = SimpleInd <$> P.between (P.char '<') (P.char '>') parseName
        parseSpecificInd = do
            _ <- P.char '<'
            pacName <- parseName
            _ <- P.char ':'
            groupName <- parseName
            _ <- P.char ':'
            indName <- parseName
            _ <- P.char '>'
            return $ SpecificInd (IndividualInfo indName [groupName] pacName)

-- turns out that we cannot easily write instances for classes, so need to be explicit for both types
instance FromJSON PoseidonEntity where parseJSON = withText "PoseidonEntity" aesonParseEntitySpec
instance FromJSON SignedEntity   where parseJSON = withText "SignedEntity" aesonParseEntitySpec
instance ToJSON   PoseidonEntity where toJSON e = String (pack $ show e)
instance ToJSON   SignedEntity   where toJSON e = String (pack $ show e)

data EntityInput a = EntitiesDirect [a] | EntitiesFromFile FilePath -- an empty list is interpreted as "all packages"

aesonParseEntitySpec :: (EntitySpec e) => Text -> Parser e
aesonParseEntitySpec t = case P.runParser entitySpecParser () "" (unpack t) of
    Left err -> fail (show err)
    Right p' -> return p'

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
        Right r  -> return r

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
        titlesRequestedPacs = nub [ pac   | Pac   pac   <- map underlyingEntity entities]
        groupNamesStats     = nub [ group | Group group <- map underlyingEntity entities]
        indNamesStats       = nub [ ind   | Ind   ind   <- map underlyingEntity entities]
        missingPacs   = map Pac   $ titlesRequestedPacs \\ titlesPac
        missingInds   = map Ind   $ filter (\x -> getIndName x `notElem` indNamesPac) indNamesStats
        missingGroups = map Group $ groupNamesStats     \\ groupNamesPac
    in  missingPacs ++ missingInds ++ missingGroups

conformingEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> [(Int, IndividualInfo)]
conformingEntityIndices entities = filter (indInfoConformsToEntitySpec entities .  snd) . zip [0..]

readEntityInputs :: (MonadIO m, EntitySpec a) => [EntityInput a] -> m [a] -- An empty list means that entities are wanted.
readEntityInputs entityInputs =
    fmap nub . fmap concat . forM entityInputs $ \entityInput -> case entityInput of
        EntitiesDirect   e  -> return e
        EntitiesFromFile fp -> liftIO $ readEntitiesFromFile fp
