module Poseidon.FStats (FStatSpec(..), fStatSpecParser, P.runParser, PopSpec(..)) where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data FStatSpec = F4Spec PopSpec PopSpec PopSpec PopSpec |
    F3Spec PopSpec PopSpec PopSpec |
    F2Spec PopSpec PopSpec |
    PairwiseMismatchSpec PopSpec PopSpec deriving (Show, Eq)

data PopSpec = PopSpecGroup String | PopSpecInd String deriving (Show, Eq)

fStatSpecParser :: P.Parser FStatSpec
fStatSpecParser = P.try f4SpecParser <|> P.try f3SpecParser <|> P.try f2SpecParser <|> pwmSpecParser

f4SpecParser :: P.Parser FStatSpec
f4SpecParser = do
    _ <- P.string "F4"
    [a, b, c, d] <- P.between (P.char '(') (P.char ')') (parsePopSpecsN 4)
    return $ F4Spec a b c d


parsePopSpecsN :: Int -> P.Parser [PopSpec]
parsePopSpecsN n = sepByN n parsePopSpec (P.char ',')

sepByN :: Int -> P.Parser a -> P.Parser sep -> P.Parser [a]
sepByN 0 _ _ = return []
sepByN 1 p _ = fmap (\x -> [x]) p
sepByN n p s = do
    x <- p
    _ <- s
    xs <- sepByN (n - 1) p s
    return (x:xs)

parsePopSpec :: P.Parser PopSpec 
parsePopSpec = parseIndividualSpec <|> parseGroupSpec
  where
    parseIndividualSpec = PopSpecInd <$> P.between (P.char '<') (P.char '>') parseName
    parseGroupSpec = PopSpecGroup <$> parseName
    parseName = P.many1 (P.satisfy (\c -> not (isSpace c || c `elem` ",<>()")))

f3SpecParser :: P.Parser FStatSpec
f3SpecParser = do
    _ <- P.string "F3"
    [a, b, c] <- P.between (P.char '(') (P.char ')') (parsePopSpecsN 3)
    return $ F3Spec a b c

f2SpecParser :: P.Parser FStatSpec
f2SpecParser = do
    _ <- P.string "F2"
    [a, b] <- P.between (P.char '(') (P.char ')') (parsePopSpecsN 2)
    return $ F2Spec a b

pwmSpecParser :: P.Parser FStatSpec
pwmSpecParser = do
    _ <- P.string "PWM"
    [a, b] <- P.between (P.char '(') (P.char ')') (parsePopSpecsN 2)
    return $ PairwiseMismatchSpec a b
