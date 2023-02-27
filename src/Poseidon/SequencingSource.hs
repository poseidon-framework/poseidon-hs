{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.SequencingSource where

import           Poseidon.Janno             (AccessionID (..),
                                             CsvNamedRecord (..),
                                             JannoList (..), JannoStringList,
                                             decodingOptions, encodingOptions,
                                             explicitNA, filterLookup, getCsvNR,
                                             removeUselessSuffix)
import           Poseidon.Utils             (PoseidonException (..), PoseidonIO,
                                             logDebug, renderPoseidonException)

import           Control.Exception          (throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, Options (..), ToJSON,
                                             defaultOptions, genericToEncoding,
                                             toEncoding)
import           Data.Bifunctor             (second)
import qualified Data.ByteString.Char8      as Bchs
import qualified Data.ByteString.Lazy.Char8 as Bch
import qualified Data.Csv                   as Csv
import           Data.Either                (lefts, rights)
import qualified Data.HashMap.Strict        as HM
import           Data.List                  (foldl', nub, sort)
import qualified Data.Vector                as V
import           GHC.Generics               (Generic)

-- | A data type to represent a seqSourceFile
newtype SeqSourceRows = SeqSourceRows [SeqSourceRow]
    deriving (Show, Eq, Generic)

instance Semigroup SeqSourceRows where
    (SeqSourceRows j1) <> (SeqSourceRows j2) = SeqSourceRows $ j1 `combineTwoSeqSources` j2
        where
        combineTwoSeqSources :: [SeqSourceRow] -> [SeqSourceRow] -> [SeqSourceRow]
        combineTwoSeqSources seqSource1 seqSource2 =
            let simpleSeqSourceSum = seqSource1 ++ seqSource2
                toAddColNames = HM.keys (HM.unions (map (getCsvNR . sAdditionalColumns) simpleSeqSourceSum))
                toAddEmptyCols = HM.fromList (map (\k -> (k, "n/a")) toAddColNames)
            in map (addEmptyAddColsToSeqSourceRow toAddEmptyCols) simpleSeqSourceSum
        addEmptyAddColsToSeqSourceRow :: Csv.NamedRecord -> SeqSourceRow -> SeqSourceRow
        addEmptyAddColsToSeqSourceRow toAdd x =
            x { sAdditionalColumns = CsvNamedRecord $ fillAddCols toAdd (getCsvNR $ sAdditionalColumns x) }
        fillAddCols :: Csv.NamedRecord -> Csv.NamedRecord -> Csv.NamedRecord
        fillAddCols toAdd cur = HM.union cur (toAdd `HM.difference` cur)

instance Monoid SeqSourceRows where
    mempty = SeqSourceRows []
    mconcat = foldl' mappend mempty

instance ToJSON SeqSourceRows where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SeqSourceRows

-- | A data type to represent a row in the seqSourceFile
-- See https://github.com/poseidon-framework/poseidon2-schema/blob/master/seqSourceFile_columns.tsv
-- for more details
data SeqSourceRow = SeqSourceRow
    { sPoseidonID                :: JannoStringList
    , sGeneticSourceAccessionIDs :: AccessionID
    , sAdditionalColumns         :: CsvNamedRecord
    }
    deriving (Show, Eq, Generic)

-- This header also defines the output column order when writing to csv!
seqSourceHeader :: [Bchs.ByteString]
seqSourceHeader = [
      "Poseidon_ID"
    , "sample_accession"
    ]

instance Csv.DefaultOrdered SeqSourceRow where
    headerOrder _ = Csv.header seqSourceHeader

seqSourceHeaderString :: [String]
seqSourceHeaderString = map Bchs.unpack seqSourceHeader

-- This hashmap represents an empty seqSourceFile with all normal, specified columns
seqSourceRefHashMap :: HM.HashMap Bchs.ByteString ()
seqSourceRefHashMap = HM.fromList $ map (\x -> (x, ())) seqSourceHeader

instance ToJSON SeqSourceRow where
    toEncoding = genericToEncoding (defaultOptions {omitNothingFields = True})

instance FromJSON SeqSourceRow

instance Csv.FromNamedRecord SeqSourceRow where
    parseNamedRecord m = SeqSourceRow
        <$> filterLookup m "Poseidon_ID"
        <*> filterLookup m "sample_accession"
        -- beyond that read everything that is not in the set of defined variables
        -- as a separate hashmap
        <*> pure (CsvNamedRecord (m `HM.difference` seqSourceRefHashMap))

instance Csv.ToNamedRecord SeqSourceRow where
    toNamedRecord s = Csv.namedRecord [
          "Poseidon_ID"      Csv..= sPoseidonID s
        , "sample_accession" Csv..= sGeneticSourceAccessionIDs s
        -- beyond that add what is in the hashmap of additional columns
        ] `HM.union` (getCsvNR $ sAdditionalColumns s)

-- | A function to write one seqSourceFile
writeSeqSourceFile :: FilePath -> SeqSourceRows -> IO ()
writeSeqSourceFile path (SeqSourceRows rows) = do
    let seqSourceAsBytestring = Csv.encodeByNameWith encodingOptions makeHeaderWithAdditionalColumns rows
    let seqSourceAsBytestringwithNA = explicitNA seqSourceAsBytestring
    Bch.writeFile path seqSourceAsBytestringwithNA
    where
        makeHeaderWithAdditionalColumns :: Csv.Header
        makeHeaderWithAdditionalColumns =
            V.fromList $ seqSourceHeader ++ sort (HM.keys (HM.unions (map (getCsvNR . sAdditionalColumns) rows)))

-- | A function to load one seqSourceFile
readSeqSourceFile :: FilePath -> PoseidonIO SeqSourceRows
readSeqSourceFile seqSourcePath = do
    logDebug $ "Reading: " ++ seqSourcePath
    seqSourceFile <- liftIO $ Bch.readFile seqSourcePath
    let seqSourceFileRows = Bch.lines seqSourceFile
    logDebug $ show (length seqSourceFileRows - 1) ++ " samples in this file"
    -- tupel with row number and row bytestring
    let seqSourceFileRowsWithNumber = zip [1..(length seqSourceFileRows)] seqSourceFileRows
    -- filter out empty lines
        seqSourceFileRowsWithNumberFiltered = filter (\(_, y) -> y /= Bch.empty) seqSourceFileRowsWithNumber
    -- create header + individual line combination
        headerOnlyPotentiallyWithQuotes = snd $ head seqSourceFileRowsWithNumberFiltered
        headerOnly = Bch.filter (/= '"') headerOnlyPotentiallyWithQuotes
        rowsOnly = tail seqSourceFileRowsWithNumberFiltered
        seqSourceFileRowsWithHeader = map (second (\x -> headerOnly <> "\n" <> x)) rowsOnly
    -- load janno by rows
    seqSourceRepresentation <- mapM (readSeqSourceFileRow seqSourcePath) seqSourceFileRowsWithHeader
    -- error case management
    if not (null (lefts seqSourceRepresentation))
    then do
        mapM_ (logDebug . renderPoseidonException) $ take 5 $ lefts seqSourceRepresentation
        liftIO $ throwIO $ PoseidonFileConsistencyException seqSourcePath "Broken lines. See more details with --logMode VerboseLog"
    else do
        let consistentSeqSource = checkSeqSourceConsistency seqSourcePath $ SeqSourceRows $ rights seqSourceRepresentation
        case consistentSeqSource of
            Left e  -> do liftIO $ throwIO (e :: PoseidonException)
            Right x -> do return x

-- | A function to load one row of a seqSourceFile
readSeqSourceFileRow :: FilePath -> (Int, Bch.ByteString) -> PoseidonIO (Either PoseidonException SeqSourceRow)
readSeqSourceFileRow seqSourcePath (lineNumber, row) = do
    case Csv.decodeByNameWith decodingOptions row of
        Left e -> do
            return $ Left $ PoseidonFileRowException seqSourcePath lineNumber $ removeUselessSuffix e
        Right (_, seqSourceRow :: V.Vector SeqSourceRow) -> do
            case checkSeqSourceRowConsistency seqSourcePath lineNumber $ V.head seqSourceRow of
                Left e                     -> do return $ Left e
                Right (pS :: SeqSourceRow) -> do return $ Right pS


checkSeqSourceConsistency :: FilePath -> SeqSourceRows -> Either PoseidonException SeqSourceRows
checkSeqSourceConsistency seqSourcePath xs
    | not $ checkSamplesUnique xs = Left $ PoseidonFileConsistencyException seqSourcePath
        "The values in the sample_accession column are not unique"
    | otherwise = Right xs

checkSamplesUnique :: SeqSourceRows -> Bool
checkSamplesUnique (SeqSourceRows rows) = length rows == length (nub $ map sGeneticSourceAccessionIDs rows)

checkSeqSourceRowConsistency :: FilePath -> Int -> SeqSourceRow -> Either PoseidonException SeqSourceRow
checkSeqSourceRowConsistency seqSourcePath row x
    | not $ checkMandatoryNotEmpty x = Left $ PoseidonFileRowException seqSourcePath row
        "The mandatory column Poseidon_ID contains empty values"
    | otherwise = Right x

checkMandatoryNotEmpty :: SeqSourceRow -> Bool
checkMandatoryNotEmpty x =
       (not . null . getJannoList . sPoseidonID $ x)
    && (not . null . head . getJannoList . sPoseidonID $ x)
