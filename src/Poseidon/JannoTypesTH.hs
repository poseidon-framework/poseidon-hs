{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE ScopedTypeVariables #-}

module Poseidon.JannoTypesTH where

import Language.Haskell.TH (Name, DecsQ, conP, conT, varP, varE, mkName, Info (..), Dec (..), Con (..), reify, conE)
import           Data.Typeable              (Typeable)
import           Data.ByteString            as S
import           Data.Aeson                 (FromJSON, ToJSON, toJSON, Value (..), parseJSON, withText)
import           Data.Aeson.Types           (Parser)
import qualified Data.Text                  as T
import qualified Data.Csv                   as Csv
import qualified Data.Text.Encoding         as T

-- a typeclass for types associated to a column name
class HasColName a where
    colname :: a -> String

-- a typeclass for types with smart constructors
class Makeable a where
    make :: MonadFail m => T.Text -> m a

-- helper functions
parseTypeCSV :: forall a m. (MonadFail m, Makeable a, HasColName a, Typeable a) => S.ByteString -> m a
parseTypeCSV x = case T.decodeUtf8' x of
        Left e  -> fail $ show e ++ " in column " ++ colname (undefined :: a)
        Right t -> make t

parseTypeJSON :: forall a. (Makeable a, HasColName a, Typeable a) => Value -> Parser a
parseTypeJSON = withText (colname (undefined :: a)) make

-- template haskell function to generate repetitive instances
makeInstances :: Name -> String -> DecsQ
makeInstances name col = do
    TyConI (NewtypeD _ _ _ _ (NormalC conName _) _) <- reify name
    let x = mkName "x"
    [d|
      instance HasColName $(conT name) where    colname _ = col
      instance Makeable $(conT name) where      make txt = return $ $(conE conName) txt
      instance Show $(conT name) where          show $(conP conName [varP x]) = T.unpack $(varE x)
      instance Csv.ToField $(conT name) where   toField $(conP conName [varP x]) = Csv.toField $(varE x)
      instance Csv.FromField $(conT name) where parseField = parseTypeCSV
      instance ToJSON $(conT name) where        toJSON ($(conP conName [varP x])) = String $(varE x)
      instance FromJSON $(conT name) where      parseJSON = parseTypeJSON
      |]
