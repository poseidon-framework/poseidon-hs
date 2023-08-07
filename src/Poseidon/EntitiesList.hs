module Poseidon.EntitiesList (
    SignedEntity (..),
    SignedEntitiesList, EntitiesList, EntitySpec,
    indInfoConformsToEntitySpec, underlyingEntity, entitySpecParser,
    readEntitiesFromFile, readEntitiesFromString,
    findNonExistentEntities, indInfoFindRelevantPackageNames, filterRelevantPackages,
    entitiesListP, EntityInput(..), readEntityInputs, PoseidonIndividual (..),
    resolveEntityIndices, SelectionLevel2 (..),     PoseidonEntity (..)) where

import           Poseidon.EntityTypes   (IndividualInfo (..),
                                         PacNameAndVersion (..),
                                         PoseidonIndividual (..),
                                         makePacNameAndVersion)
import           Poseidon.Package       (PoseidonPackage (..),
                                         getJointIndividualInfo)
import           Poseidon.Utils         (PoseidonException (..))
import           Poseidon.Version       (parseVersion)

import           Control.Applicative    ((<|>))
import           Control.Exception      (throwIO)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         withText)
import           Data.Aeson.Types       (Parser)
import           Data.Char              (isSpace)
import           Data.Function          ((&))
import           Data.List              (groupBy, nub, sort, sortBy, (\\))
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text, pack, unpack)
import qualified Text.Parsec            as P
import qualified Text.Parsec.String     as P

-- Data types for the selection process

data PoseidonEntity =
      Pac PacNameAndVersion
    | Group String
    | Ind PoseidonIndividual
    deriving (Eq, Ord)

instance Show PoseidonEntity where
    show (Pac   p) = show p
    show (Group g) = g
    show (Ind   i) = show i

instance FromJSON PoseidonEntity where parseJSON = withText "PoseidonEntity" aesonParseEntitySpec
instance ToJSON   PoseidonEntity where toJSON e = String (pack $ show e)

type EntitiesList = [PoseidonEntity]

data SignedEntity =
      Include PoseidonEntity
    | Exclude PoseidonEntity
    deriving (Eq, Ord)

instance Show SignedEntity where
    show (Include a) = show a
    show (Exclude a) = "-" ++ show a

instance FromJSON SignedEntity   where parseJSON = withText "SignedEntity" aesonParseEntitySpec
instance ToJSON   SignedEntity   where toJSON e = String (pack $ show e)

type SignedEntitiesList = [SignedEntity]

data SelectionLevel1 =
      IsInIndInfo
    | IsInIndInfoSpecified
    | IsNotInIndInfo

data SelectionLevel2 =
      ShouldBeIncluded
    | ShouldBeIncludedWithHigherPriority
    | ShouldNotBeIncluded
    deriving (Show, Eq)

meansIn :: SelectionLevel2 -> Bool
meansIn ShouldBeIncluded                   = True
meansIn ShouldBeIncludedWithHigherPriority = True
meansIn ShouldNotBeIncluded                = False

-- | A class to generalise signed and unsigned Entity Lists.
--   Both have the feature that they can be used to filter individuals.
class Eq a => EntitySpec a where
    indInfoConformsToEntitySpec :: [a] -> IndividualInfo -> SelectionLevel2
    underlyingEntity :: a -> PoseidonEntity
    entitySpecParser :: P.Parser a

instance EntitySpec SignedEntity where
    indInfoConformsToEntitySpec signedEntities indInfo@(IndividualInfo indName groupNames pacName) =
      case mapMaybe shouldIncExc signedEntities of
          [] -> ShouldNotBeIncluded
          xs -> last xs
      where
        shouldIncExc :: SignedEntity -> Maybe SelectionLevel2
        shouldIncExc (Include entity) =
            case isInIndInfo entity of
                IsInIndInfo          -> Just ShouldBeIncluded
                IsInIndInfoSpecified -> Just ShouldBeIncludedWithHigherPriority
                IsNotInIndInfo       -> Nothing
        shouldIncExc (Exclude entity) =
            case isInIndInfo entity of
                IsInIndInfo          -> Just ShouldNotBeIncluded
                IsInIndInfoSpecified -> Just ShouldNotBeIncluded
                IsNotInIndInfo       -> Nothing
        isInIndInfo :: PoseidonEntity -> SelectionLevel1
        isInIndInfo (Ind (SimpleInd n))   = if n == indName        then IsInIndInfo          else IsNotInIndInfo
        isInIndInfo (Ind (SpecificInd i)) = if i == indInfo        then IsInIndInfoSpecified else IsNotInIndInfo
        isInIndInfo (Group n)             = if n `elem` groupNames then IsInIndInfo          else IsNotInIndInfo
        isInIndInfo (Pac   n)             = if n == pacName        then IsInIndInfo          else IsNotInIndInfo
    underlyingEntity = removeEntitySign
    entitySpecParser = parseSign <*> entitySpecParser
      where
        parseSign = (P.char '-' >> return Exclude) <|> (P.optional (P.char '+') >> return Include)

instance EntitySpec PoseidonEntity where
    indInfoConformsToEntitySpec entities = indInfoConformsToEntitySpec (map Include entities)
    underlyingEntity = id
    entitySpecParser = parsePac <|> parseGroup <|> parseInd
      where
        parsePac         = Pac   <$> P.between (P.char '*') (P.char '*') parseNameAndVer
        parseGroup       = Group <$> parseName
        parseInd         = Ind   <$> (P.try parseSimpleInd <|> parseSpecificInd)
        parseNameAndVer  = PacNameAndVersion <$> parseName <*> P.optionMaybe parseMinVersion
        parseName        = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ":,<>*")))
        parseMinVersion  = do
            _ <- P.char '-'
            parseVersion
        parseSimpleInd   = SimpleInd <$> P.between (P.char '<') (P.char '>') parseName
        parseSpecificInd = do
            _ <- P.char '<'
            pac <- parseNameAndVer
            _ <- P.char ':'
            groupName <- parseName
            _ <- P.char ':'
            indName <- parseName
            _ <- P.char '>'
            return $ SpecificInd (IndividualInfo indName [groupName] pac)

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

indInfoFindRelevantPackageNames :: (EntitySpec a) => [a] -> [IndividualInfo] -> [PacNameAndVersion]
indInfoFindRelevantPackageNames e =
    nub . map indInfoPac . filter (meansIn . indInfoConformsToEntitySpec e)

filterRelevantPackages :: (EntitySpec a) => [a] -> [PoseidonPackage] -> [PoseidonPackage]
filterRelevantPackages e packages =
    let relevantPacs = indInfoFindRelevantPackageNames e (getJointIndividualInfo packages)
    in  filter (isInRelevant relevantPacs) packages
    where
        isInRelevant :: [PacNameAndVersion] -> PoseidonPackage -> Bool
        isInRelevant relPacs p = makePacNameAndVersion p `elem` relPacs

findNonExistentEntities :: (EntitySpec a) => [a] -> [IndividualInfo] -> EntitiesList
findNonExistentEntities entities individuals =
    let pacNameVer    = nub . map indInfoPac $ individuals
        indNamesPac   = map indInfoName individuals
        groupNamesPac = nub . concatMap indInfoGroups $ individuals
        requestedPacVers    = nub [ pac   | Pac   pac               <- map underlyingEntity entities]
        groupNamesStats     = nub [ group | Group group             <- map underlyingEntity entities]
        simpleIndNamesStats = nub [ ind   | Ind   (SimpleInd ind)   <- map underlyingEntity entities]
        specificIndsStats   = nub [ ind   | Ind   (SpecificInd ind) <- map underlyingEntity entities]
        missingPacs         = map Pac                 $ requestedPacVers    \\ pacNameVer
        missingGroups       = map Group               $ groupNamesStats     \\ groupNamesPac
        missingSimpleInds   = map (Ind . SimpleInd)   $ simpleIndNamesStats \\ indNamesPac
        missingSpecificInds = map (Ind . SpecificInd) $ specificIndsStats   \\ individuals
    in missingPacs ++ missingGroups ++ missingSimpleInds ++ missingSpecificInds

-- | Result: fst is a list of unresolved duplicates, snd a simple list of integers for the simple single individuals
resolveEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> ([[(Int, IndividualInfo, SelectionLevel2)]], [Int])
resolveEntityIndices entities xs =
    let allFittingIndizes = conformingEntityIndices entities xs
        groupsOfEqualNameIndividuals = resolveDuplicatesIfPossible $ groupByIndividualName allFittingIndizes
        unresolvedDuplicates = filter (\x -> length x > 1) groupsOfEqualNameIndividuals
        simpleSingles = sort $ map (\(i,_,_) -> i) $ concat $ filter (\x -> length x == 1) groupsOfEqualNameIndividuals
    in (unresolvedDuplicates, simpleSingles)
    where
        conformingEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> [(Int, IndividualInfo, SelectionLevel2)]
        conformingEntityIndices ents inds =
            filter (\(_,_,level) -> meansIn level) $ map (\(index, x) -> (index, x, indInfoConformsToEntitySpec ents x)) (zip [0..] inds)
        groupByIndividualName :: [(Int, IndividualInfo, SelectionLevel2)] -> [[(Int, IndividualInfo, SelectionLevel2)]]
        groupByIndividualName entityIndices =
            entityIndices &
                sortBy (\(_,IndividualInfo a _ _,_) (_,IndividualInfo b _ _,_) -> compare a b) &
                groupBy (\(_,IndividualInfo a _ _,_) (_,IndividualInfo b _ _,_) -> a == b)
        resolveDuplicatesIfPossible :: [[(Int, IndividualInfo, SelectionLevel2)]] -> [[(Int, IndividualInfo, SelectionLevel2)]]
        resolveDuplicatesIfPossible = map onlyKeepSpecifics
        onlyKeepSpecifics :: [(Int, IndividualInfo, SelectionLevel2)] -> [(Int, IndividualInfo, SelectionLevel2)]
        onlyKeepSpecifics groupOfInds =
            let highPrio = [ x | x@(_,_,ShouldBeIncludedWithHigherPriority) <- groupOfInds]
            in if length xs > 1 && length highPrio == 1
               then highPrio
               else groupOfInds

readEntityInputs :: (MonadIO m, EntitySpec a) => [EntityInput a] -> m [a] -- An empty list means that entities are wanted.
readEntityInputs entityInputs =
    fmap nub . fmap concat . forM entityInputs $ \entityInput -> case entityInput of
        EntitiesDirect   e  -> return e
        EntitiesFromFile fp -> liftIO $ readEntitiesFromFile fp
