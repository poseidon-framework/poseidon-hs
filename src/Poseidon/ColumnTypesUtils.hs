{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Poseidon.ColumnTypesUtils where

import           Data.ByteString     as S
import qualified Data.Csv            as Csv
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import           Data.Typeable       (Typeable)
import           Language.Haskell.TH (Con (..), Dec (..), DecsQ, Info (..),
                                      Name, conE, conP, conT, mkName, reify,
                                      varE, varP)

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
