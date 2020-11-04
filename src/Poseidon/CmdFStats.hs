module Poseidon.CmdFStats (
      FStatSpec(..)
    , fStatSpecParser
    , P.runParser
    , P.ParseError
    , PopSpec(..)
    , statSpecsFold
    , BlockData(..)
    , FstatsOptions(..)
    , JackknifeMode(..)
    , runFstats
) where

import           Poseidon.Package           (PoseidonPackage (..),
                                             getIndividuals,
                                             getJointGenotypeData,
                                             loadPoseidonPackages)
import           Poseidon.Utils             (PoseidonException (..))


import           Control.Applicative        ((<|>))
import           Control.Foldl              (Fold (..), list, purely)
import           Control.Monad              (forM, forM_)
import           Control.Monad.Catch        (throwM)
import           Data.Char                  (isSpace)
import           Data.List                  (intercalate, intersect, nub,
                                             transpose)
import           Data.Maybe                 (catMaybes)
import           Data.Vector                ((!))
import           Lens.Family2               (view)
import           Pipes                      ((>->))
import           Pipes.Group                (chunksOf, folds, groupsBy)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine)
import           SequenceFormats.Utils      (Chrom)
import           System.IO                  (hPutStrLn, stderr)
import           Text.Layout.Table          (asciiRoundS, column, def, expand,
                                             rowsG, tableString, titlesH)
import qualified Text.Parsec                as P
import qualified Text.Parsec.String         as P

data FstatsOptions = FstatsOptions
    { _foBaseDirs      :: [FilePath]
    , _foJackknifeMode :: JackknifeMode
    , _foExcludeChroms :: [Chrom]
    , _foStatSpec      :: [FStatSpec]
    , _foRawOutput     :: Bool
    }

data JackknifeMode = JackknifePerN Int
    | JackknifePerChromosome

-- | A datatype to represent Summary Statistics to be computed from genotype data.
data FStatSpec = F4Spec PopSpec PopSpec PopSpec PopSpec
    | F3Spec PopSpec PopSpec PopSpec
    | F2Spec PopSpec PopSpec
    | PWMspec PopSpec PopSpec
    deriving (Eq)

instance Show FStatSpec where
    show (F4Spec  a b c d) = "F4("  ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ ")"
    show (F3Spec  a b c  ) = "F3("  ++ show a ++ "," ++ show b ++ "," ++ show c ++ ")"
    show (F2Spec  a b    ) = "F2("  ++ show a ++ "," ++ show b ++ ")"
    show (PWMspec a b    ) = "PWM(" ++ show a ++ "," ++ show b ++ ")"

-- | An internal datatype to represent Summary statistics with indices of individuals given as integers
data FStat = F4 [Int] [Int] [Int] [Int]
    | F3 [Int] [Int] [Int]
    | F2 [Int] [Int]
    | PWM [Int] [Int]

-- | A datatype to represent a group or an individual
data PopSpec = PopSpecGroup String
    | PopSpecInd String
    deriving (Eq)

instance Show PopSpec where
    show (PopSpecGroup n) = n
    show (PopSpecInd   n) = "<" ++ n ++ ">"

-- | A helper type to represent a genomic position.
type GenomPos = (Chrom, Int)

-- | An internal datatype to represent information about a summary statistics computed for a genomic block (for Jackknifing)
data BlockData = BlockData
    { blockStartPos  :: GenomPos
    , blockEndPos    :: GenomPos
    , blockSiteCount :: Int
    , blockVal       :: Double
    }
    deriving (Show)

-- | A parser to parse Summary Statistic specifications.
fStatSpecParser :: P.Parser FStatSpec
fStatSpecParser = P.try f4SpecParser <|> P.try f3SpecParser <|> P.try f2SpecParser <|> pwmSpecParser

-- | A parser to parse F4Stats
f4SpecParser :: P.Parser FStatSpec
f4SpecParser = do
    _ <- P.string "F4"
    [a, b, c, d] <- P.between (P.char '(') (P.char ')') (parsePopSpecsN 4)
    return $ F4Spec a b c d

-- | A parser to parse exactly N different popSpecs spearated by commas
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
    step fstat (maybeStartPos, _, count, val) (EigenstratSnpEntry c p _ _ _ _, genoLine) =
        let newStartPos = case maybeStartPos of
                Nothing       -> Just (c, p)
                Just (c', p') -> Just (c', p')
            newEndPos = Just (c, p)
        in  case computeFStat fstat genoLine of
                Just v  -> (newStartPos, newEndPos, count + 1, val + v)
                Nothing -> (newStartPos, newEndPos, count + 1, val)
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
                HomRef  -> return (0 :: Integer)
                Het     -> return 1
                HomAlt  -> return 2
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

runFstats :: FstatsOptions -> IO ()
runFstats (FstatsOptions baseDirs jackknifeMode exclusionList statSpecs rawOutput) = do
    packages <- loadPoseidonPackages baseDirs
    hPutStrLn stderr $ (show . length $ packages) ++ " Poseidon packages found"
    let collectedStats = collectStatSpecGroups statSpecs
    relevantPackages <- findRelevantPackages collectedStats packages
    hPutStrLn stderr $ (show . length $ relevantPackages) ++ " relevant packages for chosen statistics identified:"
    forM_ relevantPackages $ \pac -> hPutStrLn stderr (posPacTitle pac)
    hPutStrLn stderr $ "Computing stats " ++ show statSpecs
    blockData <- runSafeT $ do
        (eigenstratIndEntries, eigenstratProd) <- getJointGenotypeData relevantPackages
        let eigenstratProdFiltered = eigenstratProd >-> P.filter chromFilter
            eigenstratProdInChunks = case jackknifeMode of
                JackknifePerChromosome  -> chunkEigenstratByChromosome eigenstratProdFiltered
                JackknifePerN chunkSize -> chunkEigenstratByNrSnps chunkSize eigenstratProdFiltered
        statsFold <- case statSpecsFold eigenstratIndEntries statSpecs of
            Left e  ->  throwM e
            Right f -> return f
        let summaryStatsProd = purely folds statsFold eigenstratProdInChunks
        purely P.fold list (summaryStatsProd >-> P.tee (P.map showBlockLogOutput >-> P.stdoutLn))
    let jackknifeEstimates = [computeJackknife (map blockSiteCount blocks) (map blockVal blocks) | blocks <- transpose blockData]
        colSpecs = replicate 4 (column expand def def def)
        tableH = ["Statistic", "Estimate", "StdErr", "Z score"]
        tableB = do
            (fstat, result) <- zip statSpecs jackknifeEstimates
            return [show fstat, show (fst result), show (snd result), show (fst result / snd result)]
    if   rawOutput
    then do
        putStrLn $ intercalate "\t" tableH
        forM_ tableB $ \row -> putStrLn (intercalate "\t" row)
    else putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]
  where
    chromFilter (EigenstratSnpEntry chrom _ _ _ _ _, _) = chrom `notElem` exclusionList
    chunkEigenstratByChromosome = view (groupsBy sameChrom)
    sameChrom (EigenstratSnpEntry chrom1 _ _ _ _ _, _) (EigenstratSnpEntry chrom2 _ _ _ _ _, _) =
        chrom1 == chrom2
    chunkEigenstratByNrSnps chunkSize = view (chunksOf chunkSize)
    showBlockLogOutput blocks = "computing chunk range " ++ show (blockStartPos (head blocks)) ++ " - " ++
        show (blockEndPos (head blocks)) ++ ", size " ++ (show . blockSiteCount . head) blocks ++ ", values " ++
        (show . map blockVal) blocks

computeJackknife :: [Int] -> [Double] -> (Double, Double)
computeJackknife weights values =
    let weights'    = map fromIntegral weights
        sumWeights  = sum weights'
        g           = fromIntegral (length weights)
        theta       = sum [mj * val | (mj, val) <- zip weights' values] / sumWeights
        sigmaSquare = sum [mj * (val - theta) ^ (2 :: Int) / (sumWeights - mj) | (mj, val) <- zip weights' values] / g
    in  (theta, sqrt sigmaSquare)

collectStatSpecGroups :: [FStatSpec] -> [PopSpec]
collectStatSpecGroups statSpecs = nub . concat $ do
    stat <- statSpecs
    case stat of
        F4Spec  a b c d -> return [a, b, c, d]
        F3Spec  a b c   -> return [a, b, c]
        F2Spec  a b     -> return [a, b]
        PWMspec a b     -> return [a, b]

findRelevantPackages :: [PopSpec] -> [PoseidonPackage] -> IO [PoseidonPackage]
findRelevantPackages popSpecs packages = do
    let indNamesStats   = [ind   | PopSpecInd   ind   <- popSpecs]
        groupNamesStats = [group | PopSpecGroup group <- popSpecs]
    fmap catMaybes . forM packages $ \pac -> do
        inds <- getIndividuals pac
        let indNamesPac   = [ind   | EigenstratIndEntry ind _ _     <- inds]
            groupNamesPac = [group | EigenstratIndEntry _   _ group <- inds]
        if   length (intersect indNamesPac indNamesStats) > 0 || length (intersect groupNamesPac groupNamesStats) > 0
        then return (Just pac)
        else return Nothing

