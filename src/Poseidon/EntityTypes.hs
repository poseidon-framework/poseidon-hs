{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.EntityTypes (
    IndividualInfo (..),
    IndividualInfoCollection,
    renderNameWithVersion,
    HasNameAndVersion (..),
    PoseidonEntity(..),
    SignedEntity(..),
    hasVersion, EntitiesList, SignedEntitiesList,
    PacNameAndVersion(..), makePacNameAndVersion, isLatestInCollection,
    EntitySpec,
    resolveUniqueEntityIndices,
    indInfoConformsToEntitySpecs, underlyingEntity, entitySpecParser,
    readEntitiesFromFile, readEntitiesFromString,
    determineNonExistentEntities, determineRelevantPackages,
    entitiesListP, EntityInput(..), readEntityInputs,
    checkIfAllEntitiesExist,
    resolveEntityIndices, reportDuplicateIndividuals) where

import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logError)
import           Poseidon.Version       (parseVersion)

import           Control.Applicative    ((<|>))
import           Control.Exception      (throwIO)
import           Control.Monad          (forM, forM_, unless, when)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         withText)
import           Data.Aeson.Types       (Parser)
import           Data.Char              (isSpace)
import           Data.List              (groupBy, intercalate, nub, sortOn)
import           Data.Maybe             (isJust, isNothing, mapMaybe)
import           Data.Text              (Text, pack, unpack)
import           Data.Version           (Version, showVersion)
import           GHC.Generics           (Generic)
import qualified Text.Parsec            as P
import qualified Text.Parsec.String     as P

-- | A class to represent a package-identifying property
class Eq a => HasNameAndVersion a where
    getPacName     :: a -> String        -- ^ a name property
    getPacVersion  :: a -> Maybe Version -- ^ a version property

-- | a convenience function
hasVersion :: (HasNameAndVersion a) => a -> Bool
hasVersion = isJust . getPacVersion

-- | universal rendering of package names and version
renderNameWithVersion :: (HasNameAndVersion a) => a -> String
renderNameWithVersion a = case getPacVersion a of
    Nothing -> getPacName a
    Just v  -> getPacName a ++ "-" ++ showVersion v

-- | a function to check whether a given package is the latest within a collection
isLatestInCollection :: (MonadThrow m, HasNameAndVersion a) => [a] -> a -> m Bool
isLatestInCollection pacCollection onePac = do
    let nameOfOne = getPacName onePac
        allWithThisName = nub $ filter ((==nameOfOne) . getPacName) $ map makePacNameAndVersion pacCollection
        -- the latest package can not be determined, if there is more than one with this name
        -- and any of them has no specified version
        missingVersion = any (isNothing . getPacVersion) allWithThisName && length allWithThisName > 1
    when missingVersion $
        throwM $ PoseidonCollectionException $
            "Can not resolve latest package, because of missing version numbers: " ++
            intercalate "," (map renderNameWithVersion allWithThisName)
    let latest = maximum allWithThisName
    return $ makePacNameAndVersion onePac == latest

-- | The minimal instance of HasNameAndVersion
data PacNameAndVersion = PacNameAndVersion {
      panavName    :: String
    , panavVersion :: Maybe Version
    }
    deriving (Ord, Eq)

instance HasNameAndVersion PacNameAndVersion where
    getPacName     = panavName
    getPacVersion  = panavVersion

instance Show PacNameAndVersion where
    show a = "*" ++ renderNameWithVersion a ++ "*"

-- | a function to normalise any instance of HasNameAndVersion to the minimal concrete type PacNameAndVersion
makePacNameAndVersion :: (HasNameAndVersion a) => a -> PacNameAndVersion
makePacNameAndVersion a = PacNameAndVersion (getPacName a) (getPacVersion a)

-- | A datatype to represent a requested package, group or individual
data PoseidonEntity =
      Pac PacNameAndVersion -- ^ all individuals in a package. A version can be specified, if not implicitly request the latest
    | Group String          -- ^ all individuals with a given group, in all of the latest packages
    | Ind String            -- ^ all individuals with the given name, in all of the latest packages
    | SpecificInd String String PacNameAndVersion -- ^ the individual specified by its name, group and package. If not versioned, then take the latest version.
    deriving (Eq, Ord)

-- | A show instance for rendering entities in forgescript
instance Show PoseidonEntity where
    show (Pac   p) = show p
    show (Group g) = g
    show (Ind   n) = "<" ++ n ++ ">"
    show (SpecificInd n g p) = "<" ++ renderNameWithVersion p ++ ":" ++ g ++ ":" ++ n ++ ">"

type EntitiesList = [PoseidonEntity]

-- | a signed entity specification, denoting inclusion or exclusion of an entity
data SignedEntity =
      Include PoseidonEntity
    | Exclude PoseidonEntity
    deriving (Eq, Ord)

instance Show SignedEntity where
    show (Include a) = show a
    show (Exclude a) = "-" ++ show a

type SignedEntitiesList = [SignedEntity]

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
            curPart  <- P.many1       (P.satisfy (\c -> not (isSpace c || c `elem` [':', ',', '<', '>', '*', '-'])))
            nextChar <- P.optionMaybe (P.satisfy (\c -> not (isSpace c || c `elem` [':', ',', '<', '>', '*'])))
            case nextChar of
                Just '-' -> do
                    isVersionComing <- probeForVersion
                    if isVersionComing
                    then return (prevPart ++ curPart)
                    else parseNamePart (prevPart ++ curPart ++ "-")
                _ -> return (prevPart ++ curPart)
        probeForVersion  = P.lookAhead (parseVersion >> return True) <|> pure False
        parseName        = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` [':', ',', '<', '>', '*'])))
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

-- turns out that we cannot easily write instances for classes, so need to be explicit for both types
instance FromJSON PoseidonEntity where parseJSON = withText "PoseidonEntity" aesonParseEntitySpec
instance FromJSON SignedEntity   where parseJSON = withText "SignedEntity" aesonParseEntitySpec
instance ToJSON   PoseidonEntity where toJSON e = String (pack $ show e)
instance ToJSON   SignedEntity   where toJSON e = String (pack $ show e)

aesonParseEntitySpec :: (EntitySpec e) => Text -> Parser e
aesonParseEntitySpec t = case P.runParser entitySpecParser () "" (unpack t) of
    Left err -> fail (show err)
    Right p' -> return p'

-- | a minimal datatype representing an individual in a collection of packages
data IndividualInfo = IndividualInfo
    { indInfoName   :: String -- ^ the name of the individual, corresponding to jPoseidonID in Janno
    , indInfoGroups :: [String] -- ^ the groups associated with the individual, corresponding to jGroupName in Janno
    , indInfoPac    :: PacNameAndVersion -- ^ the package the individual is in.
    } deriving (Show, Eq, Ord, Generic)

instance HasNameAndVersion IndividualInfo where
    getPacName       = getPacName . indInfoPac
    getPacVersion    = getPacVersion . indInfoPac

-- | a tuple of a collection of IndividualInfos and a list of bools
--   indicating whether the given sample is in the latest version of packages
type IndividualInfoCollection = ([IndividualInfo], [Bool]) 

-- data types for the selection process

data EntityInput a = EntitiesDirect [a] | EntitiesFromFile FilePath -- an empty list is interpreted as "all packages"

-- | determine all packages with versions that contain individuals covered by the given entities
determineRelevantPackages :: (MonadThrow m, EntitySpec a) => [a] -> IndividualInfoCollection -> m [PacNameAndVersion]
determineRelevantPackages entities (indInfos, areLatest) = do
    let relevantPacs = [ indInfoPac ind | (ind, l) <- zip indInfos areLatest, indInfoConformsToEntitySpecs ind l entities ]
    return . nub . map makePacNameAndVersion $ relevantPacs

-- | takes a list of selected individuals, checks for duplicates and reports a list of individuals with suggested Entity specifications
reportDuplicateIndividuals :: [IndividualInfo] -> [(IndividualInfo, [PoseidonEntity])]
reportDuplicateIndividuals individuals = do -- loop over duplication groups
    duplicateGroup@(firstInd : _) <- filter ((>1) . length) . groupBy (\a b -> indInfoName a == indInfoName b) . sortOn indInfoName $ individuals
    return (firstInd, [SpecificInd n' (head g) p | IndividualInfo n' g p <- duplicateGroup])

-- | this finds the indices of all individuals from an individual-list which are specified in the Entity list
resolveEntityIndices :: (MonadThrow m, EntitySpec a) => [a] -> IndividualInfoCollection -> m [Int]
resolveEntityIndices entities (indInfos, areLatest) = do
    let relevantIndizes = [ i | (i, ind, l) <- zip3 [0..] indInfos areLatest, indInfoConformsToEntitySpecs ind l entities ]
    return relevantIndizes

resolveUniqueEntityIndices :: (EntitySpec a) => [a] -> IndividualInfoCollection -> PoseidonIO [Int]
resolveUniqueEntityIndices entities indInfoCollection = do
    relevantIndices <- resolveEntityIndices entities indInfoCollection
    let duplicateReport = reportDuplicateIndividuals . map ((fst indInfoCollection) !!) $ relevantIndices
    -- check if there still are duplicates and if yes, then stop
    unless (null duplicateReport) $ do
        logError "There are duplicated individuals, but forge does not allow that"
        logError "Please specify in your --forgeString or --forgeFile:"
        forM_ duplicateReport $ \(IndividualInfo n _ _, specs) -> do
            logError $ "Duplicate individual " ++ show (Ind n) ++ " (please specify)"
            forM_ specs $ \spec -> do
                logError $ "  " ++ show (Ind n) ++ " -> " ++ show spec
        liftIO $ throwIO $ PoseidonForgeEntitiesException "Unresolved duplicated individuals"
    return relevantIndices

-- | this returns a list of entities which could not be found
determineNonExistentEntities :: (MonadThrow m, EntitySpec a) => [a] -> IndividualInfoCollection -> m EntitiesList
determineNonExistentEntities entities indInfoCollection = do
    return [ entity | entity <- map underlyingEntity entities, indices <- resolveEntityIndices [entity] indInfoCollection, null indices]

checkIfAllEntitiesExist :: (EntitySpec a) => [a] -> IndividualInfoCollection -> PoseidonIO ()
checkIfAllEntitiesExist entities indInfoCollection = do
    nonExistentEntities <- determineNonExistentEntities entities indInfoCollection
    unless (null nonExistentEntities) $ do
        logError "The following entities could not be found in the dataset"
        forM_ nonExistentEntities (logError . show)
        logError "Maybe these entities exist in older package versions?"
        liftIO . throwIO $ PoseidonForgeEntitiesException "some entities do not exist"

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
