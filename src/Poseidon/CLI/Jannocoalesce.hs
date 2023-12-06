{-# LANGUAGE OverloadedStrings #-}
module Poseidon.CLI.Jannocoalesce where

import           Poseidon.Janno         (JannoRow (..), JannoRows (..),
                                         readJannoFile, writeJannoFile)
import           Poseidon.Package       (PackageReadOptions (..),
                                         defaultPackageReadOptions,
                                         getJointJanno,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logInfo)

import           Control.Monad          (forM)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)

-- the source can be a single janno file, or a set of base directories as usual.
data JannoSourceSpec = JannoSourceSingle FilePath | JannoSourceBaseDirs [FilePath]

data JannoCoalesceOptions = JannoCoalesceOptions
    { _jannocoalesceSource           :: JannoSourceSpec
    , _jannocoalesceTarget           :: FilePath
    , _jannocoalesceOutSpec          :: Maybe FilePath -- Nothing means "in place"
    , _jannocoalesceFillColumns      :: [String] -- empty list means All
    , _jannocoalesceOverwriteColumns :: Bool
    }

runJannocoalesce :: JannoCoalesceOptions -> PoseidonIO ()
runJannocoalesce (JannoCoalesceOptions sourceSpec target outSpec fields overwrite) = do
    JannoRows sourceRows <- case sourceSpec of
        JannoSourceSingle sourceFile -> readJannoFile sourceFile
        JannoSourceBaseDirs sourceDirs -> do
            let pacReadOpts = defaultPackageReadOptions {
                      _readOptIgnoreChecksums      = True
                    , _readOptGenoCheck            = False
                    , _readOptIgnoreGeno           = True
                    , _readOptOnlyLatest           = True
                }
            getJointJanno <$> readPoseidonPackageCollection pacReadOpts sourceDirs
    JannoRows targetRows <- readJannoFile target
    newJanno <- forM targetRows $ \targetRow -> do
        let posId = jPoseidonID targetRow
            sourceRowCandidates = filter (\r -> jPoseidonID r == posId) sourceRows
        case sourceRowCandidates of
            [] -> return targetRow
            [keyRow] -> mergeRow targetRow keyRow fields overwrite
            _ -> throwM $ PoseidonGenericException $ "source file contains multiple rows with key " ++ posId
    let outPath = maybe target id outSpec
    logInfo $ "Writing to file (directory will be created if missing): " ++ outPath
    liftIO $ do
        createDirectoryIfMissing True (takeDirectory outPath)
        writeJannoFile outPath (JannoRows newJanno)

mergeRow :: (MonadThrow m) => JannoRow -> JannoRow -> [String] -> Bool -> m JannoRow
mergeRow targetRow sourceRow fields overwrite = do
    let targetRowRecord = Csv.toNamedRecord targetRow
        sourceRowRecord = Csv.toNamedRecord sourceRow
        parseResult = Csv.runParser . Csv.parseNamedRecord $ HM.unionWithKey mergeIfMissing targetRowRecord sourceRowRecord
    case parseResult of
        Left err -> throwM $ PoseidonGenericException $ "Janno row-merge error: " ++ err
        Right r  -> return r
  where
    mergeIfMissing :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString -> BSC.ByteString
    mergeIfMissing key targetVal sourceVal =
        if (null fields || (BSC.unpack key `elem` fields)) && (targetVal `elem` ["n/a", ""] || overwrite) then
            sourceVal
        else
            targetVal
