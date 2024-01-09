{-# LANGUAGE OverloadedStrings #-}

module Poseidon.CLI.Jannocoalesce where

import           Poseidon.Janno         (JannoRow (..), JannoRows (..),
                                         readJannoFile, writeJannoFile)
import           Poseidon.Package       (PackageReadOptions (..),
                                         defaultPackageReadOptions,
                                         getJointJanno,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logDebug, logInfo)

import           Control.Monad          (filterM, forM, forM_)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import           Data.Text              (pack, replace, unpack)
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)
import           Text.Regex.TDFA        ((=~))

-- the source can be a single janno file, or a set of base directories as usual.
data JannoSourceSpec = JannoSourceSingle FilePath | JannoSourceBaseDirs [FilePath]

data JannoCoalesceOptions = JannoCoalesceOptions
    { _jannocoalesceSource           :: JannoSourceSpec
    , _jannocoalesceTarget           :: FilePath
    , _jannocoalesceOutSpec          :: Maybe FilePath -- Nothing means "in place"
    , _jannocoalesceFillColumns      :: [String] -- empty list means All
    , _jannocoalesceOverwriteColumns :: Bool
    , _jannocoalesceSourceKey        :: String -- by default set to "Poseidon_ID"
    , _jannocoalesceTargetKey        :: String -- by default set to "Poseidon_ID"
    , _jannocoalesceIdStrip          :: Maybe String -- an optional regex to strip from target and source keys
    }

runJannocoalesce :: JannoCoalesceOptions -> PoseidonIO ()
runJannocoalesce (JannoCoalesceOptions sourceSpec target outSpec fields overwrite sKey tKey maybeStrip) = do
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

    newJanno <- makeNewJannoRows sourceRows targetRows fields overwrite sKey tKey maybeStrip

    let outPath = maybe target id outSpec
    logInfo $ "Writing to file (directory will be created if missing): " ++ outPath
    liftIO $ do
        createDirectoryIfMissing True (takeDirectory outPath)
        writeJannoFile outPath (JannoRows newJanno)

makeNewJannoRows :: [JannoRow] -> [JannoRow] -> [String] -> Bool -> String -> String -> Maybe String -> PoseidonIO [JannoRow]
makeNewJannoRows sourceRows targetRows fields overwrite sKey tKey maybeStrip = do
    logInfo "Starting to coalesce..."
    forM targetRows $ \targetRow -> do
        posId <- getKeyFromJanno targetRow tKey
        sourceRowCandidates <- filterM (\r -> (matchWithOptionalStrip maybeStrip posId) <$> getKeyFromJanno r sKey) sourceRows
        case sourceRowCandidates of
            [] -> do
                logInfo $ "no match for target " ++ posId ++ " in source"
                return targetRow
            [keyRow] -> mergeRow targetRow keyRow fields overwrite sKey tKey
            _ -> throwM $ PoseidonGenericException $ "source file contains multiple rows with key " ++ posId

getKeyFromJanno :: (MonadThrow m) => JannoRow -> String -> m String
getKeyFromJanno jannoRow key = do
    let jannoRowDict = Csv.toNamedRecord jannoRow
    case jannoRowDict HM.!? (BSC.pack key) of
        Nothing -> throwM $ PoseidonGenericException ("Key " ++ key ++ " not present in .janno file")
        Just r -> return $ BSC.unpack r

matchWithOptionalStrip :: (Maybe String) -> String -> String -> Bool
matchWithOptionalStrip maybeRegex id1 id2 =
    case maybeRegex of
        Nothing -> id1 == id2
        Just r ->
            let id1stripped = stripR r id1
                id2stripped = stripR r id2
            in  id1stripped == id2stripped
  where
    stripR :: String -> String -> String
    stripR r s =
        let match = s =~ r
        in  if null match then s else unpack $ replace (pack match) "" (pack s)

mergeRow :: JannoRow -> JannoRow -> [String] -> Bool -> String -> String -> PoseidonIO JannoRow
mergeRow targetRow sourceRow fields overwrite sKey tKey = do
    let targetRowRecord = Csv.toNamedRecord targetRow
        sourceRowRecord = Csv.toNamedRecord sourceRow
        newRowRecord    = HM.unionWithKey mergeIfMissing targetRowRecord sourceRowRecord
        parseResult     = Csv.runParser . Csv.parseNamedRecord $ newRowRecord
    logInfo $ "matched target " ++ BSC.unpack (targetRowRecord HM.! (BSC.pack tKey)) ++
              " with source " ++ BSC.unpack (sourceRowRecord HM.! (BSC.pack sKey))
    case parseResult of
        Left err -> throwM . PoseidonGenericException $ ".janno row-merge error: " ++ err
        Right r  -> do
            let newFields = HM.differenceWith (\v1 v2 -> if v1 == v2 then Nothing else Just v1) newRowRecord targetRowRecord
            if HM.null newFields then do
                logDebug "-- no changes"
            else do
                forM_ (HM.toList newFields) $ \(key, val) ->
                    logDebug $ "-- copied \"" ++ BSC.unpack val ++ "\" from column " ++ BSC.unpack key
            return r
  where
    mergeIfMissing :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString -> BSC.ByteString
    mergeIfMissing key targetVal sourceVal =
        if key /= BSC.pack tKey && (null fields || (BSC.unpack key `elem` fields)) && (targetVal `elem` ["n/a", ""] || overwrite) then
            sourceVal
        else
            targetVal
