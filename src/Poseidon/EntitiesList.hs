module Poseidon.EntitiesList (
    SignedEntity (..),
    SignedEntitiesList, EntitiesList, EntitySpec,
    indInfoConformsToEntitySpec, underlyingEntity, entitySpecParser,
    readEntitiesFromFile, readEntitiesFromString,
    findNonExistentEntities, indInfoFindRelevantPackages, filterRelevantPackages,
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
import           Data.List              (groupBy, nub, sort, sortBy, (\\), group)
import           Data.Maybe             (mapMaybe)
import           Data.Text              (Text, pack, unpack)
import qualified Text.Parsec            as P
import qualified Text.Parsec.String     as P
import Data.Version (Version)

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
      IsInIndInfo IsRequestWithVersion IsSpecified
    | IsNotInIndInfo

data IsRequestWithVersion = WithVersion Version | WithoutVersion deriving (Show, Eq)
data IsSpecified = Specified | NotSpecified deriving (Show, Eq)

data SelectionLevel2 =
      ShouldBeIncluded IsRequestWithVersion IsSpecified
    | ShouldNotBeIncluded
    deriving (Show, Eq)

meansIn :: SelectionLevel2 -> Bool
meansIn (ShouldBeIncluded _ _) = True
meansIn ShouldNotBeIncluded    = False

-- | A class to generalise signed and unsigned Entity Lists.
--   Both have the feature that they can be used to filter individuals.
class Eq a => EntitySpec a where
    indInfoConformsToEntitySpec :: [a] -> IndividualInfo -> SelectionLevel2
    underlyingEntity :: a -> PoseidonEntity
    entitySpecParser :: P.Parser a

instance EntitySpec SignedEntity where
    indInfoConformsToEntitySpec signedEntities indInfo@(IndividualInfo indName groupNames pacNameAndVer) =
      case mapMaybe shouldIncExc signedEntities of
          [] -> ShouldNotBeIncluded
          xs -> last xs
      where
        shouldIncExc :: SignedEntity -> Maybe SelectionLevel2
        shouldIncExc (Include entity) =
            case isInIndInfo entity of
                IsInIndInfo v s      -> Just $ ShouldBeIncluded v s
                IsNotInIndInfo       -> Nothing
        shouldIncExc (Exclude entity) =
            case isInIndInfo entity of
                IsInIndInfo _ _ -> Just ShouldNotBeIncluded
                IsNotInIndInfo  -> Nothing
        isInIndInfo :: PoseidonEntity -> SelectionLevel1
        isInIndInfo (Ind (SimpleInd n))   =
            if n == indName then IsInIndInfo WithoutVersion NotSpecified else IsNotInIndInfo
        isInIndInfo (Ind (SpecificInd i@(IndividualInfo _ _ (PacNameAndVersion _ Nothing)))) =
            if i `eqInd` indInfo then IsInIndInfo WithoutVersion Specified else IsNotInIndInfo
        isInIndInfo (Ind (SpecificInd i@(IndividualInfo _ _ (PacNameAndVersion _ (Just v))))) =
            if i `eqInd` indInfo then IsInIndInfo (WithVersion v) Specified else IsNotInIndInfo
        isInIndInfo (Group n) =
            if n `elem` groupNames then IsInIndInfo WithoutVersion NotSpecified else IsNotInIndInfo
        isInIndInfo (Pac p@(PacNameAndVersion _ Nothing)) =
            if p `eqPac` pacNameAndVer then IsInIndInfo WithoutVersion NotSpecified else IsNotInIndInfo
        isInIndInfo (Pac p@(PacNameAndVersion _ (Just v))) =
            if p `eqPac` pacNameAndVer then IsInIndInfo (WithVersion v) NotSpecified else IsNotInIndInfo
        eqInd :: IndividualInfo -> IndividualInfo -> Bool
        (IndividualInfo i1 g1 p1) `eqInd` (IndividualInfo i2 g2 p2) = i1 == i2 && g1 == g2 && p1 `eqPac` p2
        -- note that the LHS is the requested entity! eqPac is asymmetric!
        eqPac:: PacNameAndVersion -> PacNameAndVersion -> Bool
        (PacNameAndVersion n1 Nothing)   `eqPac` (PacNameAndVersion n2 Nothing)   = n1 == n2
        -- when a specific version is requested, then exactly this version must be matched
        (PacNameAndVersion _ (Just _))   `eqPac` (PacNameAndVersion _ Nothing)    = False
        -- when no specific version is requested, then any version is fine
        (PacNameAndVersion n1 Nothing)   `eqPac` (PacNameAndVersion n2 (Just _))  = n1 == n2
        (PacNameAndVersion n1 (Just v1)) `eqPac` (PacNameAndVersion n2 (Just v2)) = n1 == n2 && v1 == v2
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

filterRelevantPackages :: (EntitySpec a) => [a] -> [PoseidonPackage] -> [PoseidonPackage]
filterRelevantPackages entities packages =
    let relevantPacs = indInfoFindRelevantPackages entities (getJointIndividualInfo packages)
    in  filter (isInRelevant relevantPacs) packages
    where
        isInRelevant :: [PacNameAndVersion] -> PoseidonPackage -> Bool
        isInRelevant relPacs p = makePacNameAndVersion p `elem` relPacs

indInfoFindRelevantPackages :: (EntitySpec a) => [a] -> [IndividualInfo] -> [PacNameAndVersion]
indInfoFindRelevantPackages entities availableInds =
    let indsWithSelectionState = map (\ind -> (ind, indInfoConformsToEntitySpec entities ind)) availableInds
        packages = map infoPerPac indsWithSelectionState
        packagesExactly = nub [p | (p,ShouldBeIncluded (WithVersion _) _) <- packages]
        packagesUnclear = nub [p | (p,ShouldBeIncluded WithoutVersion  _) <- packages]
        packagesLatest  = map last $ group $ sort packagesUnclear
    in packagesExactly ++ packagesLatest
    where
        infoPerPac :: (IndividualInfo, SelectionLevel2) -> (PacNameAndVersion, SelectionLevel2)
        infoPerPac (IndividualInfo _ _ p, s) = (p, s)

findNonExistentEntities :: (EntitySpec a) => [a] -> [IndividualInfo] -> EntitiesList
findNonExistentEntities entities individuals =
    let pacNameVer    = nub . map indInfoPac $ individuals
        indNamesPac   = map indInfoName individuals
        groupNamesPac = nub . concatMap indInfoGroups $ individuals
        requestedPacVers    = nub [ pac | Pac   pac               <- map underlyingEntity entities]
        groupNamesStats     = nub [ grp | Group grp               <- map underlyingEntity entities]
        simpleIndNamesStats = nub [ ind | Ind   (SimpleInd ind)   <- map underlyingEntity entities]
        specificIndsStats   = nub [ ind | Ind   (SpecificInd ind) <- map underlyingEntity entities]
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
            let highPrio = [ x | x@(_,_,ShouldBeIncluded _ Specified) <- groupOfInds]
            in if length xs > 1 && length highPrio == 1
               then highPrio
               else groupOfInds

readEntityInputs :: (MonadIO m, EntitySpec a) => [EntityInput a] -> m [a] -- An empty list means that entities are wanted.
readEntityInputs entityInputs =
    fmap nub . fmap concat . forM entityInputs $ \entityInput -> case entityInput of
        EntitiesDirect   e  -> return e
        EntitiesFromFile fp -> liftIO $ readEntitiesFromFile fp
