{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Poseidon.SequencingSource where

import           Poseidon.Janno        (AccessionID (..), CsvNamedRecord (..),
                                        JannoStringList, filterLookup, getCsvNR)

import           Data.Aeson            (FromJSON, Options (..), ToJSON,
                                        defaultOptions, genericToEncoding,
                                        toEncoding)
import qualified Data.ByteString.Char8 as Bchs
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import           Data.List             (foldl')
import           GHC.Generics          (Generic)

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
