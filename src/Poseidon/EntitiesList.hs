module Poseidon.EntitiesList (
    PoseidonEntity (..), SignedEntity (..),
    SignedEntitiesList, EntitiesList,
    readSignedEntitiesFromFile, readSignedEntitiesFromString,
    entityIncludes, entityExcludes, findNonExistentEntities,
    extractEntityIndices
    ) where

import           Poseidon.Janno      (JannoList (..), JannoRow (..))
import           Poseidon.Package    (PoseidonException (PoseidonPoseidonEntityParsingException),
                                      PoseidonPackage (..), getJointJanno)
import           Poseidon.Utils      (PoseidonException (..))

import           Control.Applicative ((<|>))
import           Control.Exception   (throwIO)
import           Control.Monad       (ap)
import           Data.Char           (isSpace)
import           Data.List           ((\\))
import qualified Text.Parsec         as P
import qualified Text.Parsec.String  as P

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

isInclude :: SignedEntity -> Bool
isInclude (Include _) = True
isInclude _           = False

isExclude :: SignedEntity -> Bool
isExclude (Exclude _) = True
isExclude _           = False

fromSignedEntity :: SignedEntity -> PoseidonEntity
fromSignedEntity (Include a) = a
fromSignedEntity (Exclude a) = a

entityIncludes :: SignedEntitiesList -> EntitiesList
entityIncludes xs = map fromSignedEntity $ filter isInclude xs

entityExcludes :: SignedEntitiesList -> EntitiesList
entityExcludes xs = map fromSignedEntity $ filter isExclude xs

-- | A parser to parse entities
signedEntitiesListP :: P.Parser SignedEntitiesList
signedEntitiesListP = do
    eL <- P.sepBy signedPoseidonEntityP (P.char ',' <* P.spaces)
    P.optional (P.many (P.char ' ') >> commentP)
    return eL

signedEntitiesListMultilineP :: P.Parser SignedEntitiesList
signedEntitiesListMultilineP = do
    concat <$> P.sepBy (P.try emptyLineP <|> commentP <|> signedEntitiesListP) P.newline

commentP :: P.Parser SignedEntitiesList
commentP = do
    _ <- P.string "#"
    _ <- P.manyTill P.anyChar (P.lookAhead P.newline)
    return []

emptyLineP :: P.Parser SignedEntitiesList
emptyLineP = do
    _ <- P.manyTill (P.char ' ') (P.lookAhead P.newline)
    return []

poseidonEntityP :: P.Parser PoseidonEntity
poseidonEntityP = parsePac <|> parseGroup <|> parseInd
  where
    parsePac   = Pac   <$> P.between (P.char '*') (P.char '*') parseName
    parseGroup = Group <$> parseName
    parseInd   = Ind   <$> P.between (P.char '<') (P.char '>') parseName
    parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>*")))

signedPoseidonEntityP :: P.Parser SignedEntity
signedPoseidonEntityP = ap parseSign poseidonEntityP
  where
    parseSign = (P.char '-' >> return Exclude) <|> (P.optional (P.char '+') >> return Include)

readSignedEntitiesFromFile :: FilePath -> IO SignedEntitiesList
readSignedEntitiesFromFile entitiesFile = do
    eitherParseResult <- P.parseFromFile (signedEntitiesListMultilineP<* P.eof) entitiesFile
    case eitherParseResult of
        Left err -> throwIO (PoseidonPoseidonEntityParsingException (show err))
        Right r -> return r

readSignedEntitiesFromString :: String -> Either PoseidonException SignedEntitiesList
readSignedEntitiesFromString s = case P.runParser (signedEntitiesListP <* P.eof) () "" s of
    Left p  -> Left $ PoseidonPoseidonEntityParsingException (show p)
    Right x -> Right x

findNonExistentEntities :: EntitiesList -> [PoseidonPackage] -> IO [PoseidonEntity]
findNonExistentEntities entities packages = do
    let jannoRows = getJointJanno packages
    let titlesPac     = map posPacTitle packages
        indNamesPac   = map jPoseidonID jannoRows
        groupNamesPac = concatMap (getJannoList . jGroupName) jannoRows
    let titlesRequestedPacs = [ pac   | Pac   pac   <- entities]
        groupNamesStats     = [ group | Group group <- entities]
        indNamesStats       = [ ind   | Ind   ind   <- entities]
    let missingPacs   = map Pac   $ titlesRequestedPacs \\ titlesPac
        missingInds   = map Ind   $ indNamesStats       \\ indNamesPac
        missingGroups = map Group $ groupNamesStats     \\ groupNamesPac
    return $ missingPacs ++ missingInds ++ missingGroups

extractEntityIndices :: EntitiesList -> [PoseidonPackage] -> IO [Int]
extractEntityIndices entities packages = do
    let pacNames   = [ pac   | Pac   pac   <- entities]
        groupNames = [ group | Group group <- entities]
        indNames   = [ ind   | Ind   ind   <- entities]
    let allPackageNames = map posPacTitle packages
    allJannoRows <- getJointJanno packages
    let filterFunc (_ , pacName, EigenstratIndEntry ind _ group) =
            pacName `elem` pacNames || ind `elem` indNames || group `elem` groupNames
    return $ map extractFirst $ filter filterFunc (zipGroup allPackageNames allIndEntries)

extractFirst :: (a, b, c) -> a
extractFirst (a,_,_) = a

zipGroup :: [a] -> [[b]] -> [(Int,a,b)]
zipGroup list nestedList =
    let lenghtsNestedList = map length nestedList
        listWithlenghtsNestedList = zip lenghtsNestedList list
        longerA = map (uncurry replicate) listWithlenghtsNestedList
    in zip3 [0..] (concat longerA) (concat nestedList)
