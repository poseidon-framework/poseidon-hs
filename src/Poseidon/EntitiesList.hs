module Poseidon.EntitiesList (
    EntitySpec,
    indInfoConformsToEntitySpecs, underlyingEntity, entitySpecParser,
    readEntitiesFromFile, readEntitiesFromString,
    determineNonExistentEntities, determineRelevantPackages, filterToRelevantPackages,
    entitiesListP, EntityInput(..), readEntityInputs,
    resolveEntityIndices, reportDuplicateIndividuals) where

import           Poseidon.EntityTypes   (EntitiesList, IndividualInfo (..),
                                         PacNameAndVersion (..),
                                         PoseidonEntity (..), SignedEntity (..),
                                         isLatestInCollection,
                                         makePacNameAndVersion)
import           Poseidon.Package       (PoseidonPackage (..),
                                         getJointIndividualInfo)
import           Poseidon.Utils         (PoseidonException (..))
import           Poseidon.Version       (parseVersion)

import           Control.Applicative    ((<|>))
import           Control.Exception      (throwIO)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Char              (isSpace)
import           Data.List              (groupBy, nub, sortOn)
import           Data.Maybe             (mapMaybe)
import qualified Text.Parsec            as P
import qualified Text.Parsec.String     as P

-- data types for the selection process

data EntityInput a = EntitiesDirect [a] | EntitiesFromFile FilePath -- an empty list is interpreted as "all packages"

-- | A class to generalise signed and unsigned Entity Lists.
--   Both have the feature that they can be used to filter individuals.
class EntitySpec a where
    indInfoConformsToEntitySpec :: IndividualInfo -> Bool -> a -> Maybe Bool
    -- ^ a function to check whether a given individualInfo within a collection of individualInfos matches a given entitySpec.
    -- the second argument specifies whether the package is the latest of all possible packages in the collection
    -- `Nothing` means that the entity has no say about this individual, neither negatively nor positively
    -- `Just True` means that the entity actively selects this individual
    -- `Just False` means that the entity actively unselects this individual
    underlyingEntity :: a -> PoseidonEntity -- ^ returns the unterlying entity
    entitySpecParser :: P.Parser a -- ^ a parser

-- | this function checks whether a given individual info is selected by a given list of entities.

--  The logic is to execute the entitySpecs in order, and then use the last active call to make the decision
indInfoConformsToEntitySpecs :: (EntitySpec a) => IndividualInfo -> Bool -> [a] -> Bool
indInfoConformsToEntitySpecs indInfo isLatest entities = case mapMaybe (indInfoConformsToEntitySpec indInfo isLatest) entities of
    [] -> False
    xs -> last xs

instance EntitySpec SignedEntity where

    -- | Here we specify the exact semantics of all Includes and Excludes for all types of entities.
    -- There are only a few general patterns to exploit. We are specifying them one by one

    -- include Package
    indInfoConformsToEntitySpec (IndividualInfo _ _ p1) isLatest (Include (Pac p2)) =
        case (p1, p2) of
            (PacNameAndVersion n1 (Just v1), PacNameAndVersion n2 (Just v2)) -> if n1 == n2 && v1 == v2 then Just True else Nothing
            (PacNameAndVersion _  Nothing,   PacNameAndVersion _  (Just _ )) -> Nothing
            (PacNameAndVersion n1 _      ,   PacNameAndVersion n2 Nothing  ) -> if n1 == n2 && isLatest then Just True else Nothing

    -- exclude Package
    indInfoConformsToEntitySpec (IndividualInfo _ _ p1) _ (Exclude (Pac p2)) =
        case (p1, p2) of
            (PacNameAndVersion n1 (Just v1), PacNameAndVersion n2 (Just v2)) -> if n1 == n2 && v1 == v2 then Just False else Nothing
            (PacNameAndVersion _  Nothing,   PacNameAndVersion _  (Just _ )) -> Nothing
            (PacNameAndVersion n1 _        , PacNameAndVersion n2 Nothing)   -> if n1 == n2             then Just False else Nothing

    -- include group
    indInfoConformsToEntitySpec (IndividualInfo _ gs _) isLatest (Include (Group g)) =
        if g `elem` gs && isLatest then Just True else Nothing

    -- exclude group
    indInfoConformsToEntitySpec (IndividualInfo _ gs _) _ (Exclude (Group g)) =
        if g `elem` gs then Just False else Nothing

    -- include general individual
    indInfoConformsToEntitySpec (IndividualInfo n1 _ _ ) isLatest (Include (Ind n2)) =
        if n1 == n2 && isLatest then Just True else Nothing

    -- exclude general individual
    indInfoConformsToEntitySpec (IndividualInfo n1 _ _) _ (Exclude (Ind n2)) =
        if n1 == n2 then Just False else Nothing

    -- include specific individual
    indInfoConformsToEntitySpec indInfo@(IndividualInfo n1 gs _) isLatest (Include (SpecificInd n2 g p2)) =
        if n1 /= n2 || g `notElem` gs then Nothing else indInfoConformsToEntitySpec indInfo isLatest (Include (Pac p2))

    -- exclude specific individual
    indInfoConformsToEntitySpec indInfo@(IndividualInfo n1 gs _) isLatest (Exclude (SpecificInd n2 g p2)) =
        if n1 /= n2 || g `notElem` gs then Nothing else indInfoConformsToEntitySpec indInfo isLatest (Exclude (Pac p2))

    underlyingEntity = removeEntitySign

    entitySpecParser = parseSign <*> entitySpecParser
      where
        parseSign = (P.char '-' >> return Exclude) <|> (P.optional (P.char '+') >> return Include)

instance EntitySpec PoseidonEntity where
    indInfoConformsToEntitySpec indInfo isLatest entity = indInfoConformsToEntitySpec indInfo isLatest (Include entity)
    underlyingEntity = id
    entitySpecParser = parsePac <|> parseGroup <|> parseInd
      where
        parsePac         = Pac   <$> P.between (P.char '*') (P.char '*') parseNameAndVer
        parseGroup       = Group <$> parseName
        parseInd         = P.try parseSimpleInd <|> parseSpecificInd
        parseNameAndVer  = do
            namePart <- parseNamePart ""
            versionPart <- P.optionMaybe parseVersion
            return $ PacNameAndVersion namePart versionPart
        parseNamePart prevPart = do
            curPart  <- P.many1       (P.satisfy (\c -> not (isSpace c || c `elem` ":,<>*-")))
            nextChar <- P.optionMaybe (P.satisfy (\c -> not (isSpace c || c `elem` ":,<>*" )))
            case nextChar of
                Just '-' -> do
                    isVersionComing <- probeForVersion
                    if isVersionComing
                    then return (prevPart ++ curPart)
                    else parseNamePart (prevPart ++ curPart ++ "-")
                _ -> return (prevPart ++ curPart)
        probeForVersion  = P.lookAhead (parseVersion >> return True) <|> pure False
        parseName        = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ":,<>*")))
        parseSimpleInd   = Ind <$> P.between (P.char '<') (P.char '>') parseName
        parseSpecificInd = do
            _ <- P.char '<'
            pac <- parseNameAndVer
            _ <- P.char ':'
            groupName <- parseName
            _ <- P.char ':'
            indName <- parseName
            _ <- P.char '>'
            return $ SpecificInd indName groupName pac

-- | Filter packages such that only packages with individuals covered by the given EntitySpec are returned
filterToRelevantPackages :: (EntitySpec a) => [a] -> [PoseidonPackage] -> [PoseidonPackage]
filterToRelevantPackages entities packages =
    let relevantPacs = determineRelevantPackages entities (getJointIndividualInfo packages)
    in filter (\p -> makePacNameAndVersion p `elem` relevantPacs) packages

-- | determine all packages with versions that contain individuals covered by the given entities
determineRelevantPackages :: (EntitySpec a) => [a] -> [IndividualInfo] -> [PacNameAndVersion]
determineRelevantPackages entities indInfos = do
    indInfo <- indInfos
    let isLatest = isLatestInCollection indInfos indInfo
    True <- return $ indInfoConformsToEntitySpecs indInfo isLatest entities
    return . indInfoPac $ indInfo

-- | this finds the indices of all individuals from an individual-list which are specified in the Entity list
resolveEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> [Int]
resolveEntityIndices entities indInfos = do
    (i, indInfo) <- zip [0..] indInfos
    let isLatest = isLatestInCollection indInfos indInfo
    True <- return $ indInfoConformsToEntitySpecs indInfo isLatest entities
    return i

-- | this returns a list of entities which could not be found
determineNonExistentEntities :: (EntitySpec a) => [a] -> [IndividualInfo] -> EntitiesList
determineNonExistentEntities entities indInfos = do -- for loop over entities
    entity <- underlyingEntity <$> entities
    let indices = resolveEntityIndices [entity] indInfos
    True <- return $ null indices -- this selects only those loop iterations for which null indices is True
    return entity

-- | takes a list of selected individuals, checks for duplicates and reports a list of individuals with suggested Entity specifications
reportDuplicateIndividuals :: [IndividualInfo] -> [(IndividualInfo, [PoseidonEntity])]
reportDuplicateIndividuals individuals = do -- loop over duplication groups
    duplicateGroup@(firstInd : _) <- filter ((>1) . length) . groupBy (\a b -> indInfoName a == indInfoName b) . sortOn indInfoName $ individuals
    return (firstInd, [SpecificInd n' (head g) p | IndividualInfo n' g p <- duplicateGroup])

-- parsing code to read entities from files
readEntitiesFromString :: (EntitySpec a) => String -> Either PoseidonException [a]
readEntitiesFromString s = case P.runParser (entitiesListP <* P.eof) () "" s of
    Left p  -> Left $ PoseidonPoseidonEntityParsingException (show p)
    Right x -> Right x

readEntityInputs :: (MonadIO m, EntitySpec a, Eq a) => [EntityInput a] -> m [a] -- An empty list means that entities are wanted.
readEntityInputs entityInputs =
    fmap (nub . concat) . forM entityInputs $ \entityInput -> case entityInput of
        EntitiesDirect   e  -> return e
        EntitiesFromFile fp -> liftIO $ readEntitiesFromFile fp

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
