{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE OverloadedStrings   #-}

module Poseidon.ColumnTypesUtils where

import           Data.ByteString     as S
import qualified Data.Csv            as Csv
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Typeable       (Typeable)
import           Language.Haskell.TH (Con (..), Dec (..), DecsQ, Info (..),
                                      Name, conE, conP, conT, mkName, reify,
                                      varE, varP)
import           GHC.Generics                         (Generic)
import qualified Data.List as L
import qualified Data.ByteString.Char8                as Bchs

-- | A general datatype for janno list columns
newtype ListColumn a = ListColumn {getListColumn :: [a]}
    deriving (Eq, Ord, Generic, Show)

getMaybeListColumn :: Maybe (ListColumn a) -> [a]
getMaybeListColumn Nothing  = []
getMaybeListColumn (Just x) = getListColumn x

type JannoStringList = ListColumn String

instance (Csv.ToField a, Show a) => Csv.ToField (ListColumn a) where
    toField x = Bchs.intercalate ";" $ L.map Csv.toField $ getListColumn x
instance (Csv.FromField a) => Csv.FromField (ListColumn a) where
    parseField x = fmap ListColumn . mapM Csv.parseField $ Bchs.splitWith (==';') x

-- a typeclass for types with smart constructors
class Makeable a where
    make :: MonadFail m => T.Text -> m a

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
      instance Show $(conT name) where          show $(conP conName [varP x]) = T.unpack $(varE x)
      instance Csv.ToField $(conT name) where   toField $(conP conName [varP x]) = Csv.toField $(varE x)
      instance Csv.FromField $(conT name) where parseField = parseTypeCSV col
      |]
