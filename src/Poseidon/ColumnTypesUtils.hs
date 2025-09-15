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

import           Data.ByteString       as S
import qualified Data.ByteString.Char8 as Bchs
import           Data.Char             (chr, ord)
import qualified Data.Csv              as Csv
import qualified Data.HashMap.Strict   as HM
import qualified Data.List             as L
import qualified Data.Set              as S
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Typeable         (Typeable)
import           Generics.SOP          (All, Generic (Code, from),
                                        HCollapse (hcollapse), I (..), K (K),
                                        Proxy (..), hcmap, unSOP, unZ)
import           GHC.Generics          as G hiding (conName)
import           Language.Haskell.TH   (Con (..), Dec (..), DecsQ, Info (..),
                                        Name, conE, conP, conT, mkName, reify,
                                        varE, varP)
import qualified Text.Parsec           as P
import qualified Text.Parsec.String    as P

-- a typeclass for types with smart constructors
class Makeable a where
    make :: MonadFail m => T.Text -> m a

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
parseTypeCSV :: forall a m. (MonadFail m, Makeable a, Typeable a) => String -> S.ByteString -> m a
parseTypeCSV colname x = case T.decodeUtf8' x of
        Left e  -> fail $ show e ++ " in column " ++ colname
        Right t -> make $ T.strip t

-- template haskell function to generate repetitive instances
makeInstances :: Name -> String -> DecsQ
makeInstances name col = do
    TyConI (NewtypeD _ _ _ _ (NormalC conName _) _) <- reify name
    let x = mkName "x"
    [d|
      instance Makeable $(conT name) where      make txt = return $ $(conE conName) txt
      instance Suspicious $(conT name) where    inspect _ = Nothing
      instance Show $(conT name) where          show $(conP conName [varP x]) = T.unpack $(varE x)
      instance Csv.ToField $(conT name) where   toField $(conP conName [varP x]) = Csv.toField $(varE x)
      instance Csv.FromField $(conT name) where parseField = parseTypeCSV col
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

-- | A datatype to collect additional, unpecified .csv/.tsv file columns (a hashmap in cassava/Data.Csv)
newtype CsvNamedRecord = CsvNamedRecord Csv.NamedRecord deriving (Show, Eq, G.Generic)

getCsvNR :: CsvNamedRecord -> Csv.NamedRecord
getCsvNR (CsvNamedRecord x) = x

-- helper functions for .csv/.tsv reading
filterLookup :: Csv.FromField a => Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser a
filterLookup m name = case cleanInput $ HM.lookup name m of
    Nothing -> fail $ "Missing value in mandatory column " <> Bchs.unpack name
    Just x  -> Csv.parseField  x

filterLookupOptional :: Csv.FromField a => S.Set Bchs.ByteString -> Csv.NamedRecord -> Bchs.ByteString -> Csv.Parser (Maybe a)
filterLookupOptional mandatory m name = -- maybe (pure Nothing) Csv.parseField . cleanInput $ HM.lookup name m
    case cleanInput $ HM.lookup name m of
        Nothing -> if name `S.member` mandatory
                   then fail $ "Missing value in mandatory column " <> Bchs.unpack name
                   else pure Nothing
        Just x  -> Csv.parseField x

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
instance (Csv.FromField a) => Csv.FromField (ListColumn a) where
    parseField x = fmap ListColumn . mapM Csv.parseField $ Bchs.splitWith (==';') x

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

