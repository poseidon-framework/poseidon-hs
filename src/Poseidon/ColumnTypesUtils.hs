{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
-- the following ones are necessary for the generics-sop magic
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeOperators       #-}

module Poseidon.ColumnTypesUtils where

import qualified Control.Monad.Except     as E
import qualified Control.Monad.Writer     as W
import           Data.ByteString          as S
import qualified Data.ByteString.Char8    as Bchs
import           Data.Char                (chr, ord)
import qualified Data.Csv                 as Csv
import qualified Data.HashMap.Strict      as HM
import qualified Data.List                as L
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Typeable            (Typeable)
import           Generics.SOP             (All, Generic (Code, from),
                                           HCollapse (hcollapse), I (..), K (K),
                                           Proxy (..), hcmap, unSOP, unZ)
import           GHC.Generics             as G hiding (conName)
import           Language.Haskell.TH      (Con (..), Dec (..), DecsQ, Info (..),
                                           Name, conE, conP, conT, mkName,
                                           reify, varE, varP)
import           Poseidon.PoseidonVersion (PoseidonVersion)
import qualified Text.Parsec              as P
import qualified Text.Parsec.String       as P

-- a typeclass like cassavas FromField, but aware of PoseidonVersions
class FromFieldVersioned a where
    parseFieldVersioned :: PoseidonVersion -> Csv.Field -> Csv.Parser a

instance FromFieldVersioned String where parseFieldVersioned _ = Csv.parseField
instance FromFieldVersioned Int where parseFieldVersioned _ = Csv.parseField
-- modified from cassava's FromField (Maybe a) instance
instance FromFieldVersioned a => FromFieldVersioned (Maybe a) where
    parseFieldVersioned pv s
        | Bchs.null s  = pure Nothing
        | otherwise = Just <$> parseFieldVersioned pv s

-- a typeclass for types with smart constructors
class Makeable a where
    make :: MonadFail m => PoseidonVersion -> T.Text -> m a

-- a typeclass for .csv/.tsv column types that may require a logged warning when read
class Suspicious a where
    inspect :: a -> Maybe [String]

instance Suspicious String where
    inspect _ = Nothing
instance Suspicious a => Suspicious (Maybe a) where
    inspect Nothing  = Nothing
    inspect (Just x) = inspect x
instance Suspicious a => Suspicious (ListColumn a) where
    inspect (ListColumn xs) = L.concat <$> mapM inspect xs
instance Suspicious CsvNamedRecord where
    inspect _ =  Nothing

-- generics-sop magic to inspect all fields of a JannoRow or SeqSourceRow with one command
inspectEachField :: (Generics.SOP.Generic a, Code a ~ '[ xs ], All Suspicious xs) => a -> [Maybe [String]] --'
inspectEachField =
      hcollapse
    . hcmap (Proxy :: Proxy Suspicious) (\(I x) -> K $ inspect x)
    . unZ . unSOP . Generics.SOP.from

-- helper functions
parseTypeCSV :: forall a m. (MonadFail m, Makeable a, Typeable a) => PoseidonVersion -> String -> S.ByteString -> m a
parseTypeCSV pv colname x = case T.decodeUtf8' x of
        Left e  -> fail $ show e ++ " in column " ++ colname
        Right t -> make pv $ T.strip t

-- template haskell function to generate repetitive instances
makeInstances :: Name -> String -> DecsQ
makeInstances name col = do
    TyConI (NewtypeD _ _ _ _ (NormalC conName _) _) <- reify name
    let x = mkName "x"
    [d|
      instance Makeable $(conT name) where      make _ txt = return $ $(conE conName) txt
      instance Suspicious $(conT name) where    inspect _ = Nothing
      instance Show $(conT name) where          show $(conP conName [varP x]) = T.unpack $(varE x)
      instance Csv.ToField $(conT name) where   toField $(conP conName [varP x]) = Csv.toField $(varE x)
      instance FromFieldVersioned $(conT name) where parseFieldVersioned pv = parseTypeCSV pv col
      |]

-- general encoding/decoding options
decodingOptions :: Csv.DecodeOptions
decodingOptions = Csv.defaultDecodeOptions {
    Csv.decDelimiter = fromIntegral (ord '\t')
}
encodingOptions :: Csv.EncodeOptions
encodingOptions = Csv.defaultEncodeOptions {
      Csv.encDelimiter = fromIntegral (ord '\t')
    , Csv.encUseCrLf = False
    , Csv.encIncludeHeader = True
    , Csv.encQuoting = Csv.QuoteMinimal
}

-- | A data type for row-wise cross-column consistency checks in either .janno or .ssf
type RowLog = E.ExceptT String (W.Writer [String])
-- first string: error in case of failure
-- string list: warnings

getCellLength :: Maybe (ListColumn a) -> Int
getCellLength = maybe 0 (Prelude.length . getListColumn)

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual x  = Prelude.length (L.nub x) == 1

-- | A datatype to collect additional, unpecified .csv/.tsv file columns (a hashmap in cassava/Data.Csv)
newtype CsvNamedRecord = CsvNamedRecord Csv.NamedRecord deriving (Show, Eq, G.Generic)

getCsvNR :: CsvNamedRecord -> Csv.NamedRecord
getCsvNR (CsvNamedRecord x) = x

-- helper functions for .csv/.tsv reading
checkMandatory :: Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser ()
checkMandatory m mandatory =
  case cleanInput (HM.lookup mandatory m) of
    Nothing -> fail ("Missing value in mandatory column: " <> Bchs.unpack mandatory)
    Just _  -> pure ()

filterLookup :: FromFieldVersioned a => PoseidonVersion -> Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup pv m name = case cleanInput $ HM.lookup name m of
    Nothing -> fail $ "Missing value in mandatory column: " <> Bchs.unpack name
    Just x  -> parseFieldVersioned pv x

filterLookupOptional :: FromFieldVersioned a => PoseidonVersion -> Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional pv m name = maybe (pure Nothing) (\bs -> Just <$> parseFieldVersioned pv bs) . cleanInput $ HM.lookup name m

cleanInput :: Maybe Bchs.ByteString -> Maybe Bchs.ByteString
cleanInput Nothing           = Nothing
cleanInput (Just rawInputBS) = transNA rawInputBS
    where
        transNA :: Bchs.ByteString -> Maybe Bchs.ByteString
        transNA ""    = Nothing
        transNA "n/a" = Nothing
        transNA x     = Just x

explicitNA :: Csv.NamedRecord -> Csv.NamedRecord
explicitNA = HM.map (\x -> if Bchs.null x then "n/a" else x)

-- | A general datatype for janno list columns
newtype ListColumn a = ListColumn {getListColumn :: [a]}
    deriving (Eq, Ord, G.Generic, Show)

getMaybeListColumn :: Maybe (ListColumn a) -> [a]
getMaybeListColumn Nothing  = []
getMaybeListColumn (Just x) = getListColumn x

instance (Csv.ToField a, Show a) => Csv.ToField (ListColumn a) where
    toField x = Bchs.intercalate ";" $ L.map Csv.toField $ getListColumn x
instance (FromFieldVersioned a) => FromFieldVersioned (ListColumn a) where
    parseFieldVersioned pv x = fmap ListColumn . mapM (parseFieldVersioned pv) $ Bchs.splitWith (==';') x

-- helper functions for reformatting parser errors
removeUselessSuffix :: String -> String
removeUselessSuffix = T.unpack . T.replace " at \"\"" "" . T.pack

data CsvParseError = CsvParseError {
      _expected :: String
    , _actual   :: String
    , _leftover :: String
} deriving Show

parseCsvParseError :: P.Parser CsvParseError
parseCsvParseError = do
    _ <- P.string "parse error (Failed reading: conversion error: expected "
    expected <- P.manyTill P.anyChar (P.try (P.string ", got "))
    actual <- P.manyTill P.anyChar (P.try (P.string " (incomplete field parse, leftover: ["))
    leftoverList <- P.sepBy (read <$> P.many1 P.digit) (P.char ',')
    _ <- P.char ']'
    _ <- P.many P.anyChar
    return $ CsvParseError expected actual (L.map chr leftoverList)

renderCsvParseError :: CsvParseError -> String
renderCsvParseError (CsvParseError expected actual leftover) =
    "parse error in one column (" ++
    "expected data type: " ++ expected ++ ", " ++
    "broken value: " ++ actual ++ ", " ++
    "problematic characters: " ++ show leftover ++ ")"

