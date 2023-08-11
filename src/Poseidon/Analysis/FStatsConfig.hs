{-# LANGUAGE OverloadedStrings #-}
module Poseidon.Analysis.FStatsConfig where

import           Poseidon.Analysis.Utils (GroupDef, XerxesException (..),
                                          parseGroupDefsFromJSON)

import           Control.Applicative     ((<|>))
import           Control.Exception       (throwIO)
import           Control.Monad           (forM, when)
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson              (FromJSON (..), withObject, withText,
                                          (.:), (.:?))
import qualified Data.ByteString         as B
import           Data.Char               (isSpace)
import           Data.Yaml               (decodeEither')
import           Poseidon.EntitiesList   (PoseidonEntity (..),
                                          PoseidonIndividual (..))
import           Poseidon.SecondaryTypes (IndividualInfo (..))
import qualified Text.Parsec             as P
import qualified Text.Parsec.String      as P
import           Text.Read               (readMaybe)


-- import qualified Dhall as D

-- | A datatype to represent different types of F-Statistics
data FStatType = F4
    | F3
    | F2
    | PWM
    | Het
    | FST
    | F3vanilla
    | F2vanilla
    | FSTvanilla
    deriving (Show, Read, Eq)

-- | A datatype to represent F-Statistics to be computed from genotype data.
data FStatSpec = FStatSpec FStatType [PoseidonEntity] (Maybe AscertainmentSpec)
    deriving (Eq, Show)

data AscertainmentSpec = AscertainmentSpec
    { ascOutgroupSpec  :: Maybe PoseidonEntity -- Here, a Nothing denotes that minor allele frequency in ascRefgroup should be used,
    --otherwise use derived allele frequencies in the ref-group, where the ancestral allele is given by the outgroup
    , ascRefgroupSpec  :: PoseidonEntity
    , ascLowerFreqSpec :: Double
    , ascUpperFreqSpec :: Double
    }
    deriving (Show, Eq)

data FStatInput = FStatInputDirect FStatSpec
    | FStatInputYaml FilePath
    | FStatInputSimpleText FilePath

data FStatConfigYaml = FStatConfigYaml
    { fcGroupDefs  :: [GroupDef]
    , fcFStatSpecs :: [MultiFStatSpec]
    }
    deriving (Show)

data MultiFStatSpec = MultiFStatSpec
    { mFstatType          :: FStatType
    , mFstatSlotA         :: [PoseidonEntity]
    , mFstatSlotB         :: [PoseidonEntity]
    , mFstatSlotC         :: [PoseidonEntity]
    , mFstatSlotD         :: [PoseidonEntity]
    , mFstatAscertainment :: Maybe AscertainmentSpec
    }
    deriving (Show)

instance FromJSON FStatConfigYaml where
    parseJSON = withObject "FStatConfigYaml" $ \v -> FStatConfigYaml
        <$> (v .:? "groupDefs" >>= maybe (return []) parseGroupDefsFromJSON)
        <*>  v .:  "fstats"

instance FromJSON MultiFStatSpec where
    parseJSON = withObject "fstats" $ \v -> MultiFStatSpec
        <$> v .:  "type"
        <*> v .:  "a"
        <*> ((v .:? "b") >>= maybe (return []) return)
        <*> ((v .:? "c") >>= maybe (return []) return)
        <*> ((v .:? "d") >>= maybe (return []) return)
        <*> v .:? "ascertainment"

instance FromJSON AscertainmentSpec where
    parseJSON = withObject "Ascertainment" $ \v -> AscertainmentSpec
        <$> v .:? "outgroup"
        <*> v .:  "reference"
        <*> v .:  "lower"
        <*> v .:  "upper"

instance FromJSON FStatType where
    parseJSON = withText "FStatType" $ \t -> case t of
        "F4"         -> return F4
        "F3"         -> return F3
        "F2"         -> return F2
        "FST"        -> return FST
        "PWM"        -> return PWM
        "Het"        -> return Het
        "F3vanilla"  -> return F3vanilla
        "FSTvanilla" -> return FSTvanilla
        "F2vanilla"  -> return F2vanilla
        _            -> fail "could not parse FStat type. Must be one of F4, F3, F2, FST, PWM, Het, F3vanilla, FSTvanilla, F2vanilla"


processYamlConfig :: FStatConfigYaml -> Either XerxesException ([GroupDef], [FStatSpec])
processYamlConfig (FStatConfigYaml groupDefs multiFstats) = do
    let eitherFStats = do
            MultiFStatSpec t aList bList cList dList maybeAsc <- multiFstats
            case fstatSlotLength t of
                1 ->
                    if null aList then
                        [Left . FStatException $ "a can't be empty for " ++ show t ++ " statistics"]
                    else
                        [Right $ FStatSpec t [a] maybeAsc | a <- aList]
                2 ->
                    if null aList || null bList then
                        [Left . FStatException $ "a and b can't be empty for " ++ show t ++ " statistics"]
                    else
                        [Right $ FStatSpec t [a, b] maybeAsc | a <- aList, b <- bList]
                3 -> do
                    if null aList || null bList || null cList then
                        [Left . FStatException $ "a, b and c can't be empty for " ++ show t ++ " statistics"]
                    else
                        [Right $ FStatSpec t [a, b, c] maybeAsc | a <- aList, b <- bList, c <- cList]
                4 -> do
                    if null aList || null bList || null cList || null dList then
                        [Left . FStatException $ "a, b, c and d can't be empty for " ++ show t ++ " statistics"]
                    else
                        [Right $ FStatSpec t [a, b, c, d] maybeAsc | a <- aList, b <- bList, c <- cList, d <- dList]
                _ -> error "should never happen"
    fStats <- sequence eitherFStats
    return (groupDefs, fStats)

readFstatsYamlConfig :: (MonadIO m) => FilePath -> m ([GroupDef], [FStatSpec])
readFstatsYamlConfig fn = do
    bs <- liftIO $ B.readFile fn
    case decodeEither' bs of
        Left err -> liftIO . throwIO $ PopConfigYamlException fn (show err)
        Right x  -> case processYamlConfig x of
            Left e  -> liftIO . throwIO $ e
            Right c -> return c

-- | A parser to parse Summary Statistic specifications from the simple text file input. Every line is one statistics. No ascertainment can be given with this interface.
fStatSpecParser :: P.Parser FStatSpec
fStatSpecParser = do
    typeStr <- P.many1 (P.satisfy (\c -> c /= '('))
    fStatType <- case readMaybe typeStr of
        Just t -> return t
        Nothing -> fail $ "Cannot parse Statistic type " ++ typeStr ++
            ". Must be one of F4, F3, F2, FST, PWM, Het, F3vanilla, FSTvanilla"
    slots <- P.between (P.char '(') (P.char ')') parseEntities
    when (not (fstatSlotLength fStatType == length slots)) $
        fail $ "Not the right number of arguments to Statistic " ++ show fStatType
    return $ FStatSpec fStatType slots Nothing -- no ascertainment can be specified using this input interface.
  where
    parseEntities = P.sepBy1 customEntitySpecParser (P.char ',' <* P.spaces)
    customEntitySpecParser = parsePac <|> parseGroup <|> parseInd
      where
        parsePac   = Pac   <$> P.between (P.char '*') (P.char '*') parseName
        parseGroup = Group <$> parseName
        parseInd   = Ind   <$> (P.try parseSimpleInd <|> parseSpecificInd)
        parseName  = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` charList)))
        charList = ":,<>*()" :: [Char] -- we use a custom parser here, because we cannot tolerate bracket openings and closings,
            -- which is something that is not constrained in Poseidon.EntityList.
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


fstatSlotLength :: FStatType -> Int
fstatSlotLength fStatType = case fStatType of
    F4         -> 4
    F3         -> 3
    F2         -> 2
    FST        -> 2
    PWM        -> 2
    Het        -> 1
    F3vanilla  -> 3
    FSTvanilla -> 2
    F2vanilla  -> 2

readFStatsSimpleText :: (MonadIO m) => FilePath -> m [FStatSpec]
readFStatsSimpleText statSpecsFile = do
    let multiFstatSpecParser = fStatSpecParser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- liftIO $ P.parseFromFile (P.spaces *> multiFstatSpecParser <* P.spaces) statSpecsFile
    case eitherParseResult of
        Left err -> liftIO . throwIO $ FStatException (show err)
        Right r  -> return r

readFstatInput :: (MonadIO m) => [FStatInput] -> m ([GroupDef], [FStatSpec])
readFstatInput inputSources = do
    listOfPairs <- forM inputSources $ \inputSource -> do
        case inputSource of
            FStatInputDirect fStat  -> return ([], [fStat])
            FStatInputYaml fn       -> readFstatsYamlConfig fn
            FStatInputSimpleText fn -> (\f -> ([], f)) <$> readFStatsSimpleText fn
    return (concatMap fst listOfPairs, concatMap snd listOfPairs)

------------ DHALL implementation, experimental and commented out for now -------------------
-- readFstatConfigDhall :: FilePath -> IO FStatConfig
-- readFstatConfigDhall = D.inputFile fStatD

-- fStatD :: D.Decoder FStatConfig
-- fStatD = D.record (
--     FStatConfig <$> D.field "groupDefs" (D.list groupDefD)
--                 <*> D.field "fStatSpecs" (D.list fStatSpecD))

-- groupDefD :: D.Decoder GroupDef
-- groupDefD = D.record (
--     (,) <$> D.field "name" D.string <*> D.field "entities" (D.list signedEntityD))
--   where
--     signedEntityD = (Include . Group) <$> D.string

-- fStatSpecD :: D.Decoder FStatSpec
-- fStatSpecD = D.record (
--     FStatSpec <$> D.field "type" fTypeD
--               <*> D.field "slots" (D.list entityD)
--               <*> D.field "ascertainment" (D.maybe ascertainmentSpecD))
--   where
--     entityD = Group <$> D.string
--     fTypeD = (const F3) <$> D.string

-- ascertainmentSpecD :: D.Decoder AscertainmentSpec
-- ascertainmentSpecD = D.record (
--     AscertainmentSpec <$> D.field "outgroup" (D.maybe (Group <$> D.string))
--                       <*> D.field "refgroup" (Group <$> D.string)
--                       <*> D.field "lofreq" D.double
--                       <*> D.field "hifreq" D.double)
