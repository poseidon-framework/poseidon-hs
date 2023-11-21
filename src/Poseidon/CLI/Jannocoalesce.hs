{-# LANGUAGE OverloadedStrings #-}
module Poseidon.CLI.Jannocoalesce where

import Poseidon.Janno (JannoRow(..), JannoRows(..), writeJannoFile, readJannoFile)
import Poseidon.Utils (PoseidonIO, PoseidonException(..))

import Control.Monad (forM)
import Control.Monad.Catch (throwM, MonadThrow)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM

data JannoCoalesceOptions = JannoCoalesceOptions
    { _jannocoalesceSource           :: FilePath
    , _jannocoalesceTarget           :: FilePath
    , _jannocoalesceOutSpec          :: Maybe FilePath -- Nothing means "in place"
    , _jannocoalesceFillColumns      :: [String] -- empty list means All
    , _jannocoalesceOverwriteColumns :: Bool
    }

runJannocoalesce :: JannoCoalesceOptions -> PoseidonIO ()
runJannocoalesce (JannoCoalesceOptions source target outSpec fields overwrite) = do
    JannoRows sourceRows <- readJannoFile source
    JannoRows targetRows <- readJannoFile target
    newJanno <- forM targetRows $ \targetRow -> do
        let posId = jPoseidonID targetRow
            sourceRowCandidates = filter (\r -> jPoseidonID r == posId) sourceRows
        case sourceRowCandidates of
            [] -> return targetRow
            [keyRow] -> mergeRow targetRow keyRow fields overwrite
            _ -> throwM $ PoseidonGenericException $ "source file contains multiple rows with key " ++ posId
    let outPath = maybe target id outSpec 
    liftIO $ writeJannoFile outPath (JannoRows newJanno)


mergeRow :: (MonadThrow m) => JannoRow -> JannoRow -> [String] -> Bool -> m JannoRow
mergeRow targetRow sourceRow fields overwrite = do
    let targetRowRecord = Csv.toNamedRecord targetRow
        sourceRowRecord = Csv.toNamedRecord sourceRow
        parseResult = Csv.runParser . Csv.parseNamedRecord $ HM.unionWithKey mergeIfMissing targetRowRecord sourceRowRecord
    case parseResult of
        Left err -> throwM $ PoseidonGenericException $ "Janno row-merge error: " ++ err
        Right r  -> return r
  where
    mergeIfMissing key targetVal sourceVal =
        if (null key || (BSC.unpack key `elem` fields)) && (targetVal `elem` ["n/a", ""] || overwrite) then
            sourceVal
        else
            targetVal
