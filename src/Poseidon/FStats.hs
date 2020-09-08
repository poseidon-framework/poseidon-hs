module Poseidon.FStats where

import Text.Parsec.String (Parser)

data FStatSpec = F4Spec PopSpec PopSpec PopSpec PopSpec |
    F3Spec PopSpec PopSpec PopSpec |
    F2Spec PopSpec PopSpec |
    PairwiseMismatchSpec PopSpec PopSpec

data PopSpec = PopSpecGroup String | PopSpecInd String

fStatSpecParser :: Parser FStatSpec
fStatSpecParser = f4SpecParser <|> f3SpecParser <|> f2SpecParser <|> pwmSpecParser

f4SpecParser :: Parser FStatSpec
f4SpecParser = do
    _ <- P.string "F4"
    _ <- P.between (P.char "(") (P.char ")") (parsePopSpecs 4)

parsePopSpecsN :: Int -> Parser [PopSpec]
parsePopSpecsN n = sepByN n parsePopSpec (P.char ',')

sepByN :: Int -> Parser a -> Parser sep -> Parser [a]
sepByN 0 _ _ = return []
sepByN 1 p _ = fmap (\x -> return [x]) p
sepByN n p s = do
    x <- p
    _ <- s
    xs <- sepByN (n - 1) p s
    return (reverse (x:xs))

parsePopSpec :: P.Parser PopSpec 
parsePopSpec = parseIndividualSpec <|> parseGroupSpec
  where
    parseIndividualSpec = PopSpecInd <$> P.between (P.char '<') (P.char '>') parseName
    parseGroupSpec = PopSpecGroup <$> parseName
    parseName = P.takeTill isSpace

f3SpecParser :: Parser FStatSpec
f3SpecParser = do
    _ <- P.string "F3"
    _ <- P.between (P.char "(") (P.char ")") (parsePopSpecs 3)

f2SpecParser :: Parser FStatSpec
f2SpecParser = do
    _ <- P.string "F2"
    _ <- P.between (P.char "(") (P.char ")") (parsePopSpecs 2)

pwmSpecParser :: Parser FStatSpec
pwmSpecParser = do
    _ <- P.string "PWM"
    _ <- P.between (P.char "(") (P.char ")") (parsePopSpecs 2)
