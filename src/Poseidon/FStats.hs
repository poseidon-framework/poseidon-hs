module Poseidon.FStats (FStatSpec(..), fStatSpecParser, P.runParser, P.ParseError, PopSpec(..), statSpecsFold) where

import Poseidon.Utils (PoseidonException(..))

import Control.Applicative ((<|>))
import Control.Foldl (Fold(..))
import Data.Char (isSpace)
import Data.Vector ((!))
import SequenceFormats.Eigenstrat (EigenstratSnpEntry(..), GenoLine, EigenstratIndEntry(..), GenoEntry(..))
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

data FStatSpec = F4Spec PopSpec PopSpec PopSpec PopSpec |
    F3Spec PopSpec PopSpec PopSpec |
    F2Spec PopSpec PopSpec |
    PWMspec PopSpec PopSpec deriving (Show, Eq)

data FStat = F4 [Int] [Int] [Int] [Int] |
    F3 [Int] [Int] [Int] |
    F2 [Int] [Int] |
    PWM [Int] [Int]

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
    return $ PWMspec a b

statSpecsFold :: [EigenstratIndEntry] -> [FStatSpec] -> Either PoseidonException (Fold (EigenstratSnpEntry, GenoLine) (Int, [Double]))
statSpecsFold indEntries fStatSpecs = do
    listOfFolds <- mapM (statSpecFold indEntries) fStatSpecs
    let foldOfList = sequenceA listOfFolds
    return $ fmap (\list -> (fst (head list), map snd list)) foldOfList

statSpecFold :: [EigenstratIndEntry] -> FStatSpec -> Either PoseidonException (Fold (EigenstratSnpEntry, GenoLine) (Int, Double))
statSpecFold iE fStatSpec = do
    fStat <- case fStatSpec of
        F4Spec  a b c d -> F4  <$> getPopIndices iE a <*> getPopIndices iE b <*> getPopIndices iE c <*> getPopIndices iE d
        F3Spec  a b c   -> F3  <$> getPopIndices iE a <*> getPopIndices iE b <*> getPopIndices iE c
        F2Spec  a b     -> F2  <$> getPopIndices iE a <*> getPopIndices iE b
        PWMspec a b     -> PWM <$> getPopIndices iE a <*> getPopIndices iE b
    return $ Fold (step fStat) initialize extract
  where
    step :: FStat -> (Int, Double) -> (EigenstratSnpEntry, GenoLine) -> (Int, Double)
    step fStat (count, val) (_, genoLine) =
        case computeFStat fStat genoLine of
            Just v  -> (count + 1, val + v)
            Nothing -> (count + 1, val)
    initialize :: (Int, Double)
    initialize = (0, 0.0)
    extract :: (Int, Double) -> (Int, Double)
    extract = id

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


