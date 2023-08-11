module Poseidon.Generator.Parsers where

import           Poseidon.Generator.Types
import           Poseidon.Generator.Utils

import           Poseidon.Janno

import           Control.Exception        (throwIO)
import           Control.Monad            (guard)
import           Data.List                (intercalate)
import           Data.Ratio               ((%))
import qualified Text.Parsec              as P
import qualified Text.Parsec.Number       as P
import qualified Text.Parsec.String       as P

renderRequestedInds :: [RequestedInd] -> String
renderRequestedInds requestedInds =
    let indString = intercalate ";" $ map show $ take 5 requestedInds
    in if length requestedInds > 5
       then indString ++ "..."
       else indString

readIndWithAdmixtureSetString :: String -> Either String [RequestedInd]
readIndWithAdmixtureSetString s = case P.runParser indWithAdmixtureSetMultiParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

readIndWithAdmixtureSetFromFile :: FilePath -> IO [RequestedInd]
readIndWithAdmixtureSetFromFile file = do
    let multiParser = indWithAdmixtureSetMultiParser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- P.parseFromFile (P.spaces *> multiParser <* P.spaces) file
    case eitherParseResult of
        Left err -> throwIO $ PoseidonGeneratorCLIParsingException (show err)
        Right r  -> return (concat r)

indWithAdmixtureSetMultiParser :: P.Parser [RequestedInd]
indWithAdmixtureSetMultiParser = P.try (P.sepBy parseIndWithAdmixtureSet (P.char ';' <* P.spaces))

parseIndWithAdmixtureSet :: P.Parser RequestedInd
parseIndWithAdmixtureSet = do
    _ <- P.oneOf "["
    indP <- P.manyTill P.anyChar (P.string ":")
    unitP <- P.manyTill P.anyChar (P.string "]")
    _ <- P.oneOf "("
    setP <- populationWithFractionMultiParser
    _ <- P.oneOf ")"
    return (RequestedInd indP unitP setP)

populationWithFractionMultiParser :: P.Parser [PopFrac]
populationWithFractionMultiParser = P.try (P.sepBy parsePopulationWithFraction (P.char '+' <* P.spaces))

parsePopulationWithFraction :: P.Parser PopFrac
parsePopulationWithFraction = do
    popP <- P.many (P.noneOf "=")
    _ <- P.oneOf "="
    percP <- read <$> P.many1 P.digit
    return (PopFrac popP (percP % 100))

readIndWithPositionString :: String -> Either String [IndWithPosition]
readIndWithPositionString s = case P.runParser indWithPositionParser () "" s of
    Left p  -> Left (show p)
    Right x -> Right x

readIndWithPositionFromFile :: FilePath -> IO [IndWithPosition]
readIndWithPositionFromFile positionFile = do
    let multiPositionParser = indWithPositionParser `P.sepBy1` (P.newline *> P.spaces)
    eitherParseResult <- P.parseFromFile (P.spaces *> multiPositionParser <* P.spaces) positionFile
    case eitherParseResult of
        Left err -> throwIO $ PoseidonGeneratorCLIParsingException (show err)
        Right r  -> return (concat r)

indWithPositionParser :: P.Parser [IndWithPosition]
indWithPositionParser = P.try (P.sepBy parseIndWithPosition (P.char ';' <* P.spaces))

parseIndWithPosition :: P.Parser IndWithPosition
parseIndWithPosition = do
    _ <- P.oneOf "["
    ind <- P.manyTill P.anyChar (P.string ":")
    unit <- P.manyTill P.anyChar (P.string "]")
    _ <- P.oneOf "("
    spatpos <- parseSpatialTemporalPosition
    _ <- P.oneOf ")"
    return (IndWithPosition ind unit spatpos)

parseSpatialTemporalPosition :: P.Parser SpatialTemporalPosition
parseSpatialTemporalPosition = do
    timeP <- pInt
    _ <- P.oneOf ","
    latP <- pLat
    _ <- P.oneOf ","
    lonP <- pLon
    return (SpatialTemporalPosition timeP latP lonP)

pInt :: P.Parser Int
pInt = read <$> P.many1 P.digit

pLat :: P.Parser Latitude
pLat = do
    latP <- P.sign <*> P.floating2 True
    guard (latP >= -90 && latP <= 90) P.<?> "valid latitude (-90 to 90)"
    return (Latitude latP)

pLon :: P.Parser Longitude
pLon = do
    lonP <- P.sign <*> P.floating2 True
    guard (lonP >= -180 && lonP <= 180) P.<?> "valid longitude (-180 to 180)"
    return (Longitude lonP)
