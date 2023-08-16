{-# LANGUAGE ViewPatterns #-}

module Poseidon.EntitiesList (
    SignedEntity (..),
    SignedEntitiesList, EntitiesList, EntitySpec,
    indInfoConformsToEntitySpec, underlyingEntity, entitySpecParser,
    readEntitiesFromFile, readEntitiesFromString,
    determineNonExistentEntities, determineRelevantPackages, filterToRelevantPackages,
    entitiesListP, EntityInput(..), readEntityInputs, PoseidonIndividual (..),
    resolveEntityIndices, SelectionState (..), PoseidonEntity (..), IsSpecified (..)) where

import           Poseidon.EntityTypes   (HasNameAndVersion (..),
                                         IndividualInfo (..),
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

-- data types for the selection process

data EntityInput a = EntitiesDirect [a] | EntitiesFromFile FilePath -- an empty list is interpreted as "all packages"

data PoseidonEntity =
      Pac PacNameAndVersion
    | Group String
    | Ind PoseidonIndividual
    deriving (Ord)

instance Eq PoseidonEntity where
    (==) (Pac (PacNameAndVersion n1 v1)) (Pac (PacNameAndVersion n2 v2)) = n1 == n2 && v1 == v2
    (==) (Group g1)                      (Group g2)                      = g1 == g2
    (==) (Ind i1)                        (Ind i2)                        = i1 == i2
    (==) _                               _                               = False

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

instance FromJSON SignedEntity where parseJSON = withText "SignedEntity" aesonParseEntitySpec
instance ToJSON   SignedEntity where toJSON e = String (pack $ show e)

type SignedEntitiesList = [SignedEntity]

data SelectionState =
      ShouldBeIncluded PacNameAndVersion IsSpecified
    | ShouldNotBeIncluded
    deriving (Show, Eq)

meansIn :: SelectionState -> Bool
meansIn (ShouldBeIncluded _ _) = True
meansIn ShouldNotBeIncluded    = False

data IsSpecified = Specified | NotSpecified deriving (Show, Eq)

isSpecified :: SelectionState -> Bool
isSpecified (ShouldBeIncluded _ Specified) = True
isSpecified _ = False

-- | A class to generalise signed and unsigned Entity Lists.
--   Both have the feature that they can be used to filter individuals.
class Eq a => EntitySpec a where
    indInfoConformsToEntitySpec :: [a] -> IndividualInfo -> [SelectionState]
    underlyingEntity :: a -> PoseidonEntity
    entitySpecParser :: P.Parser a

instance EntitySpec SignedEntity where
    indInfoConformsToEntitySpec
        signedEntities
        indInfo@(IndividualInfo indName groupNames pacNameAndVer@(PacNameAndVersion pacName _)) =
      takeWhile meansIn $ reverse $ mapMaybe shouldIncExc signedEntities -- this considers the entity order in forgeScript
      where
        shouldIncExc :: SignedEntity -> Maybe SelectionState
        shouldIncExc (Include entity) = case isIn entity of
                ShouldBeIncluded p s -> Just $ ShouldBeIncluded p s
                ShouldNotBeIncluded  -> Nothing
        shouldIncExc (Exclude entity) = case isIn entity of
                ShouldBeIncluded _ _ -> Just ShouldNotBeIncluded
                ShouldNotBeIncluded  -> Nothing
        isIn :: PoseidonEntity -> SelectionState
        isIn (Ind (SimpleInd n))   =
            if n == indName then ShouldBeIncluded (PacNameAndVersion pacName Nothing) NotSpecified else ShouldNotBeIncluded
        isIn (Ind (SpecificInd i@(IndividualInfo _ _ (PacNameAndVersion _ Nothing)))) =
            if i `eqInd` indInfo then ShouldBeIncluded (PacNameAndVersion pacName Nothing) Specified else ShouldNotBeIncluded
        isIn (Ind (SpecificInd i@(IndividualInfo _ _ p@(PacNameAndVersion _ (Just _))))) =
            if i `eqInd` indInfo then ShouldBeIncluded p Specified else ShouldNotBeIncluded
        isIn (Group n) =
            if n `elem` groupNames then ShouldBeIncluded (PacNameAndVersion pacName Nothing) NotSpecified else ShouldNotBeIncluded
        isIn (Pac p@(PacNameAndVersion _ Nothing)) =
            if p `eqPac` pacNameAndVer then ShouldBeIncluded (PacNameAndVersion pacName Nothing) NotSpecified else ShouldNotBeIncluded
        isIn (Pac p@(PacNameAndVersion _ (Just _))) =
            if p `eqPac` pacNameAndVer then ShouldBeIncluded p NotSpecified else ShouldNotBeIncluded
        eqInd :: IndividualInfo -> IndividualInfo -> Bool
        -- head should be fine here, because the SpecificInd parser reads exactly one group name
        (IndividualInfo i1 g1 p1) `eqInd` (IndividualInfo i2 g2 p2) = i1 == i2 && (head g1) `elem` g2 && p1 `eqPac` p2
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

-- entity filter logic

filterToRelevantPackages :: (EntitySpec a) => [a] -> [PoseidonPackage] -> [PoseidonPackage]
filterToRelevantPackages entities packages =
    let relevantPacs = determineRelevantPackages entities (getJointIndividualInfo packages)
    in filter (\p -> makePacNameAndVersion p `elem` relevantPacs) packages

determineRelevantPackages :: (EntitySpec a) => [a] -> [IndividualInfo] -> [PacNameAndVersion]
determineRelevantPackages entities availableInds =
    let selectionStates = concatMap (indInfoConformsToEntitySpec entities) availableInds
        packagesExactly = nub [p | (ShouldBeIncluded p@(PacNameAndVersion _ (Just _)) _) <- selectionStates]
        packagesUnclear = nub [pacName | (ShouldBeIncluded (PacNameAndVersion pacName Nothing) _)  <- selectionStates]
        packagesLatest  = map getLatestPackageVersion packagesUnclear
    in packagesExactly ++ packagesLatest --error $ show packagesExactly ++ " + " ++ show packagesLatest
    where
        getLatestPackageVersion :: String -> PacNameAndVersion
        getLatestPackageVersion n = maximum $ filter (\p -> n == getPacName p) $ map makePacNameAndVersion availableInds

determineNonExistentEntities :: (EntitySpec a) => [a] -> [IndividualInfo] -> EntitiesList
determineNonExistentEntities entities availableInds =
    let pacNameVer    = nub . map indInfoPac $ availableInds
        indNamesPac   = map indInfoName availableInds
        groupNamesPac = nub . concatMap indInfoGroups $ availableInds
        requestedPacVers    = nub [ pac | Pac   pac               <- map underlyingEntity entities]
        groupNamesStats     = nub [ grp | Group grp               <- map underlyingEntity entities]
        simpleIndNamesStats = nub [ ind | Ind   (SimpleInd ind)   <- map underlyingEntity entities]
        specificIndsStats   = nub [ ind | Ind   (SpecificInd ind) <- map underlyingEntity entities]
        missingPacs         = map Pac                 $ requestedPacVers    \\ pacNameVer
        missingGroups       = map Group               $ groupNamesStats     \\ groupNamesPac
        missingSimpleInds   = map (Ind . SimpleInd)   $ simpleIndNamesStats \\ indNamesPac
        missingSpecificInds = map (Ind . SpecificInd) $ specificIndsStats   \\ availableInds
    in missingPacs ++ missingGroups ++ missingSimpleInds ++ missingSpecificInds

-- This function requires [IndividualInfo] to be from a clean package selection as prepared with filterToRelevantPackages!
-- | Result: fst is a list of unresolved duplicates, snd a simple list of integers for the simple single individuals
resolveEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> ([[(Int, IndividualInfo, [SelectionState])]], [Int])
resolveEntityIndices entities xs =
    let allFittingIndizes = conformingEntityIndices entities xs
        groupsOfEqualNameIndividuals = resolveDuplicatesIfPossible $ groupByIndividualName allFittingIndizes
        unresolvedDuplicates = filter (\x -> length x > 1) groupsOfEqualNameIndividuals
        simpleSingles = sort $ map (\(i,_,_) -> i) $ concat $ filter (\x -> length x == 1) groupsOfEqualNameIndividuals
    in (unresolvedDuplicates, simpleSingles)
    where
        conformingEntityIndices :: (EntitySpec a) => [a] -> [IndividualInfo] -> [(Int, IndividualInfo, [SelectionState])]
        conformingEntityIndices ents inds =
            filter (\(_,_,level) -> not $ null level) $ map (\(index, x) -> (index, x, indInfoConformsToEntitySpec ents x)) (zip [0..] inds)
        groupByIndividualName :: [(Int, IndividualInfo, [SelectionState])] -> [[(Int, IndividualInfo, [SelectionState])]]
        groupByIndividualName entityIndices =
            entityIndices &
                sortBy (\(_,IndividualInfo a _ _,_) (_,IndividualInfo b _ _,_) -> compare a b) &
                groupBy (\(_,IndividualInfo a _ _,_) (_,IndividualInfo b _ _,_) -> a == b)
        resolveDuplicatesIfPossible :: [[(Int, IndividualInfo, [SelectionState])]] -> [[(Int, IndividualInfo, [SelectionState])]]
        resolveDuplicatesIfPossible = map onlyKeepSpecifics
        onlyKeepSpecifics :: [(Int, IndividualInfo, [SelectionState])] -> [(Int, IndividualInfo, [SelectionState])]
        onlyKeepSpecifics groupOfInds =
            let highPrio = filter (\(_,_,level) -> any isSpecified level) groupOfInds
            in if length xs > 1 && length highPrio == 1
               then highPrio
               else groupOfInds

-- parsing code to read entities from files

readEntitiesFromString :: (EntitySpec a) => String -> Either PoseidonException [a]
readEntitiesFromString s = case P.runParser (entitiesListP <* P.eof) () "" s of
    Left p  -> Left $ PoseidonPoseidonEntityParsingException (show p)
    Right x -> Right x

readEntityInputs :: (MonadIO m, EntitySpec a) => [EntityInput a] -> m [a] -- An empty list means that entities are wanted.
readEntityInputs entityInputs =
    fmap nub . fmap concat . forM entityInputs $ \entityInput -> case entityInput of
        EntitiesDirect   e  -> return e
        EntitiesFromFile fp -> liftIO $ readEntitiesFromFile fp

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
