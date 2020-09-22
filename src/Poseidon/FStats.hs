module Poseidon.FStats (FStatSpec(..), fStatSpecParser, P.runParser, P.ParseError, PopSpec(..), statSpecsFold,
BlockData(..)) where

import Poseidon.Utils (PoseidonException(..))

import Control.Applicative ((<|>))
import Control.Foldl (Fold(..))
import Data.Char (isSpace)
import Data.Vector ((!))
import SequenceFormats.Eigenstrat (EigenstratSnpEntry(..), GenoLine, EigenstratIndEntry(..), GenoEntry(..))
import SequenceFormats.Utils (Chrom)
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data FStatSpec = F4Spec PopSpec PopSpec PopSpec PopSpec |
    F3Spec PopSpec PopSpec PopSpec |
    F2Spec PopSpec PopSpec |
    PWMspec PopSpec PopSpec deriving (Eq)

instance Show FStatSpec where
    show (F4Spec  a b c d) = "F4("  ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ ")"
    show (F3Spec  a b c  ) = "F3("  ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"
    show (F2Spec  a b    ) = "F2("  ++ show a ++ "," ++ show b ++ ")"
    show (PWMspec a b    ) = "PWM(" ++ show a ++ "," ++ show b ++ ")"

data FStat = F4 [Int] [Int] [Int] [Int] |
    F3 [Int] [Int] [Int] |
    F2 [Int] [Int] |
    PWM [Int] [Int]

data PopSpec = PopSpecGroup String | PopSpecInd String deriving (Eq)

instance Show PopSpec where
    show (PopSpecGroup n) = n
    show (PopSpecInd   n) = "<" ++ n ++ ">"

type GenomPos = (Chrom, Int)

data BlockData = BlockData {
    blockStartPos :: GenomPos,
    blockEndPos :: GenomPos,
    blockSiteCount :: Int,
    blockVal :: Double
} deriving (Show)

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
    return $ PWMspec a b

statSpecsFold :: [EigenstratIndEntry] -> [FStatSpec] -> Either PoseidonException (Fold (EigenstratSnpEntry, GenoLine) [BlockData])
statSpecsFold indEntries fStatSpecs = do
    listOfFolds <- mapM (statSpecFold indEntries) fStatSpecs
    return $ sequenceA listOfFolds

statSpecFold :: [EigenstratIndEntry] -> FStatSpec -> Either PoseidonException (Fold (EigenstratSnpEntry, GenoLine) BlockData)
statSpecFold iE fStatSpec = do
    fStat <- case fStatSpec of
        F4Spec  a b c d -> F4  <$> getPopIndices iE a <*> getPopIndices iE b <*> getPopIndices iE c <*> getPopIndices iE d
        F3Spec  a b c   -> F3  <$> getPopIndices iE a <*> getPopIndices iE b <*> getPopIndices iE c
        F2Spec  a b     -> F2  <$> getPopIndices iE a <*> getPopIndices iE b
        PWMspec a b     -> PWM <$> getPopIndices iE a <*> getPopIndices iE b
    return $ Fold (step fStat) initialize extract
  where
    step :: FStat -> (Maybe GenomPos, Maybe GenomPos, Int, Double) ->
        (EigenstratSnpEntry, GenoLine) -> (Maybe GenomPos, Maybe GenomPos, Int, Double)
    step fstat (maybeStartPos, maybeEndPos, count, val) (EigenstratSnpEntry c p _ _ _ _, genoLine) =
        let newStartPos = case maybeStartPos of
                Nothing -> Just (c, p)
                Just (c', p') -> Just (c', p')
            newEndPos = Just (c, p)
        in  case computeFStat fstat genoLine of
                Just v  -> (newStartPos, newEndPos, count + 1, val + v)
                Nothing -> (newStartPos, newEndPos, count, val)
    initialize :: (Maybe GenomPos, Maybe GenomPos, Int, Double)
    initialize = (Nothing, Nothing, 0, 0.0)
    extract :: (Maybe GenomPos, Maybe GenomPos, Int, Double) -> BlockData
    extract (maybeStartPos, maybeEndPos, count, totalVal) =
        let Just startPos = maybeStartPos
            Just endPos = maybeEndPos
        in  BlockData startPos endPos count (totalVal / fromIntegral count)

computeFStat :: FStat -> GenoLine -> Maybe Double
computeFStat fStat gL = case fStat of
    (F4  aI bI cI dI) -> computeF4  <$> computeFreq gL aI <*> computeFreq gL bI <*> computeFreq gL cI <*> computeFreq gL dI
    (F3  aI bI cI   ) -> computeF3  <$> computeFreq gL aI <*> computeFreq gL bI <*> computeFreq gL cI
    (F2  aI bI      ) -> computeF2  <$> computeFreq gL aI <*> computeFreq gL bI
    (PWM aI bI      ) -> computePWM <$> computeFreq gL aI <*> computeFreq gL bI
  where
    computeF4  a b c d = (a - b) * (c - d)
    computeF3  a b c   = (c - a) * (c - b)
    computeF2  a b     = (a - b) * (a - b)
    computePWM a b     = a * (1.0 - b) + (1.0 - a) * b

computeFreq :: GenoLine -> [Int] -> Maybe Double
computeFreq line indices =
    let nrNonMissing = length . filter (/=Missing) . map (line !) $ indices
        nrDerived = sum $ do
            i <- indices
            case line ! i of
                HomRef -> return (0 :: Integer)
                Het -> return 1
                HomAlt -> return 2
                Missing -> return 0
    in  if nrNonMissing > 0
        then Just (fromIntegral nrDerived / fromIntegral nrNonMissing)
        else Nothing

getPopIndices :: [EigenstratIndEntry] -> PopSpec -> Either PoseidonException [Int]
getPopIndices indEntries popSpec =
    let ret = do
            (i, EigenstratIndEntry indName _ popName) <- zip [0..] indEntries
            True <- case popSpec of 
                PopSpecGroup name -> return (name == popName)
                PopSpecInd   name -> return (name == indName)
            return i
    in  if (null ret)
        then case popSpec of
            PopSpecGroup n -> Left $ PoseidonIndSearchException ("Group name " ++ n ++ " not found")
            PopSpecInd   n -> Left $ PoseidonIndSearchException ("Individual name " ++ n ++ " not found")
        else Right ret


