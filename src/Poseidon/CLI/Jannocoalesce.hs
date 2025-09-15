{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Poseidon.CLI.Jannocoalesce where

import           Poseidon.Janno         (JannoRow (..), JannoRows (..),
                                         parseJannoRowFromNamedRecord,
                                         readJannoFile, writeJannoFile)
import           Poseidon.Package       (PackageReadOptions (..),
                                         defaultPackageReadOptions,
                                         getJointJanno,
                                         readPoseidonPackageCollection)
import           Poseidon.Utils         (PoseidonException (..), PoseidonIO,
                                         logDebug, logInfo, logWarning)

import           Control.Monad          (filterM, forM_, when)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BSC
import qualified Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import qualified Data.IORef             as R
import           Data.List              ((\\))
import qualified Data.Set               as S
import           Data.Text              (pack, replace, unpack)
import           System.Directory       (createDirectoryIfMissing)
import           System.FilePath        (takeDirectory)
import           Text.Regex.TDFA        ((=~))

-- the source can be a single janno file, or a set of base directories as usual.
data JannoSourceSpec = JannoSourceSingle FilePath | JannoSourceBaseDirs [FilePath]

data CoalesceJannoColumnSpec =
      AllJannoColumns
    | IncludeJannoColumns [BSC.ByteString]
    | ExcludeJannoColumns [BSC.ByteString]

data JannoCoalesceOptions = JannoCoalesceOptions
    { _jannocoalesceSource           :: JannoSourceSpec
    , _jannocoalesceTarget           :: FilePath
    , _jannocoalesceOutSpec          :: Maybe FilePath -- Nothing means "in place"
    , _jannocoalesceJannoColumns     :: CoalesceJannoColumnSpec
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

type CounterMismatches = R.IORef Int
type CounterCopied     = R.IORef Int

makeNewJannoRows :: [JannoRow] -> [JannoRow] -> CoalesceJannoColumnSpec -> Bool -> String -> String -> Maybe String -> PoseidonIO [JannoRow]
makeNewJannoRows sourceRows targetRows fields overwrite sKey tKey maybeStrip = do
    logInfo "Starting to coalesce..."
    counterMismatches <- liftIO $ R.newIORef 0
    counterCopied <- liftIO $ R.newIORef 0
    newRows <- mapM (makeNewJannoRow counterMismatches counterCopied) targetRows
    counterCopiedVal <- liftIO $ R.readIORef counterCopied
    counterMismatchesVal <- liftIO $ R.readIORef counterMismatches
    logInfo $ "Copied " ++ show counterCopiedVal ++ " values"
    when (counterMismatchesVal > 0) $
        logWarning $ "Failed to find matches for " ++ show counterMismatchesVal ++ " target rows in source"
    return newRows
    where
        makeNewJannoRow :: CounterMismatches -> CounterCopied -> JannoRow -> PoseidonIO JannoRow
        makeNewJannoRow cm cp targetRow = do
            posId <- getKeyFromJanno targetRow tKey
            sourceRowCandidates <- filterM (\r -> (matchWithOptionalStrip maybeStrip posId) <$> getKeyFromJanno r sKey) sourceRows
            case sourceRowCandidates of
                [] -> do
                    logWarning $ "no match for target " ++ posId ++ " in source"
                    liftIO $ R.modifyIORef cm (+1)
                    return targetRow
                [keyRow] -> mergeRow cp targetRow keyRow fields overwrite sKey tKey
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

mergeRow :: CounterCopied -> JannoRow -> JannoRow -> CoalesceJannoColumnSpec -> Bool -> String -> String -> PoseidonIO JannoRow
mergeRow cp targetRow sourceRow fields overwrite sKey tKey = do
    let sourceKeys        = HM.keys sourceRowRecord
        sourceKeysDesired = determineDesiredSourceKeys sourceKeys fields
        -- fill in the target row with dummy values for desired fields that might not be present yet
        targetComplete    = HM.union targetRowRecord (HM.fromList $ map (, BSC.empty) sourceKeysDesired)
        newRowRecord      = HM.mapWithKey fillFromSource targetComplete
        parseResult       = Csv.runParser . (parseJannoRowFromNamedRecord S.empty) $ newRowRecord
    logInfo $ "matched target " ++ BSC.unpack (targetComplete  HM.! BSC.pack tKey) ++
              " with source "   ++ BSC.unpack (sourceRowRecord HM.! BSC.pack sKey)
    case parseResult of
        Left err -> throwM . PoseidonGenericException $ ".janno row-merge error: " ++ err
        Right r  -> do
            let newFields = HM.differenceWith (\v1 v2 -> if v1 == v2 then Nothing else Just v1) newRowRecord targetComplete
            if HM.null newFields then do
                logDebug "-- no changes"
            else do
                forM_ (HM.toList newFields) $ \(key, val) -> do
                    liftIO $ R.modifyIORef cp (+1)
                    logDebug $ "-- copied \"" ++ BSC.unpack val ++ "\" from column " ++ BSC.unpack key
            return r
  where
    targetRowRecord :: Csv.NamedRecord
    targetRowRecord = Csv.toNamedRecord targetRow
    sourceRowRecord :: Csv.NamedRecord
    sourceRowRecord = Csv.toNamedRecord sourceRow
    determineDesiredSourceKeys :: [BSC.ByteString] -> CoalesceJannoColumnSpec -> [BSC.ByteString]
    determineDesiredSourceKeys keys  AllJannoColumns               = keys
    determineDesiredSourceKeys _    (IncludeJannoColumns included) = included
    determineDesiredSourceKeys keys (ExcludeJannoColumns excluded) = keys \\ excluded
    fillFromSource :: BSC.ByteString -> BSC.ByteString -> BSC.ByteString
    fillFromSource key targetVal =
           -- don't overwrite key
        if key /= BSC.pack tKey
           -- overwrite field only if it's requested
           && includeField key fields
           -- overwrite only empty fields, except overwrite is set
           && (targetVal `elem` ["n/a", "", BSC.empty] || overwrite)
        then HM.findWithDefault "" key sourceRowRecord
        else targetVal
    includeField :: BSC.ByteString -> CoalesceJannoColumnSpec -> Bool
    includeField _    AllJannoColumns         = True
    includeField key (IncludeJannoColumns xs) = key `elem` xs
    includeField key (ExcludeJannoColumns xs) = key `notElem` xs
