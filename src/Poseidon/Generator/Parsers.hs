module Poseidon.Generator.Parsers where

import           Poseidon.Generator.Types
import           Poseidon.Generator.Utils

import           Control.Exception        (throwIO)
import           Data.List                (intercalate)
import           Data.Ratio               ((%))
import qualified Text.Parsec              as P
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
