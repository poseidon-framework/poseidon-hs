{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Utils (
    PoseidonException (..),
    renderPoseidonException,
    usePoseidonLogger,
    testLog,
    PoseidonLogIO,
    LogMode (..),
    checkFile,
    getChecksum,
    logWarning,
    logInfo,
    logDebug,
    logError,
    LogEnv,
    noLog,
    logWithEnv,
    padRight, padLeft
) where

import           Colog                  (HasLog (..), LogAction (..), Message,
                                         Msg (..), Severity (..), cfilter,
                                         cmapM, logTextStderr, msgSeverity,
                                         msgText, showSeverity)
import           Control.Exception      (Exception)
import           Control.Exception.Base (SomeException)
import           Control.Monad          (when)
import           Control.Monad.Catch    (throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, asks, runReaderT)
import qualified Data.ByteString.Lazy   as LB
import           Data.Digest.Pure.MD5   (md5)
import           Data.Text              (Text, pack)
import           Data.Time              (defaultTimeLocale, formatTime,
                                         getCurrentTime, utcToLocalZonedTime)
import           Data.Yaml              (ParseException)
import           GHC.Stack              (callStack, withFrozenCallStack)
import           System.Directory       (doesFileExist)

type LogEnv = LogAction IO Message

type PoseidonLogIO = ReaderT LogEnv IO

data LogMode = NoLog
    | SimpleLog
    | DefaultLog
    | ServerLog
    | VerboseLog
    deriving Show

usePoseidonLogger :: LogMode -> PoseidonLogIO a -> IO a
usePoseidonLogger NoLog      = flip runReaderT noLog
usePoseidonLogger SimpleLog  = flip runReaderT simpleLog
usePoseidonLogger DefaultLog = flip runReaderT defaultLog
usePoseidonLogger ServerLog  = flip runReaderT serverLog
usePoseidonLogger VerboseLog = flip runReaderT verboseLog

testLog :: PoseidonLogIO a -> IO a
testLog = usePoseidonLogger NoLog

noLog      :: LogEnv
noLog      = cfilter (const False) simpleLog
simpleLog  :: LogEnv
simpleLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg False False
defaultLog :: LogEnv
defaultLog = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True False
serverLog  :: LogEnv
serverLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True True
verboseLog :: LogEnv
verboseLog = compileLogMsg True True

compileLogMsg :: Bool -> Bool -> LogEnv
compileLogMsg severity time = cmapM prepareMessage logTextStderr
    where
        prepareMessage :: Message -> IO Text
        prepareMessage msg = do
            let textMessage = msgText msg
                textSeverity = if severity
                    then showSeverity (msgSeverity msg)
                    else mempty
            textTime <- if time
                    then do
                        zonedTime <- getCurrentTime >>= utcToLocalZonedTime
                        return $ pack $ "[" ++ formatTime defaultTimeLocale "%T" zonedTime ++ "] "
                    else mempty
            return $ textSeverity <> textTime <> textMessage

logMsg :: Severity -> String -> PoseidonLogIO ()
logMsg sev msg = do
    {-
    Using asks getLogAction here gives us a bit of flexibility. If in the future we'd like to expand the
    ReaderT environment by adding more options or parameters to LogEnv, perhaps even the command line options,
    we can do so, we just need to adapt the HasLog instance, which tells us how to get the logAction out of the
    environment.
    -}
    LogAction logF <- asks getLogAction
    liftIO . withFrozenCallStack . logF $ Msg sev callStack (pack msg)

logWarning :: String -> PoseidonLogIO ()
logWarning = logMsg Warning

logInfo :: String -> PoseidonLogIO ()
logInfo = logMsg Info

logDebug :: String -> PoseidonLogIO ()
logDebug = logMsg Debug

logError :: String -> PoseidonLogIO ()
logError = logMsg Error

logWithEnv :: (MonadIO m) => LogEnv -> PoseidonLogIO () -> m ()
logWithEnv logEnv = liftIO . flip runReaderT logEnv

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException =
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonPackageVersionException FilePath String -- ^ An exception to represent an issue with a package version
    | PoseidonPackageMissingVersionException FilePath -- ^ An exception to indicate a missing poseidonVersion field
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors in the genotype data
    | PoseidonGenotypeExceptionForward SomeException -- ^ An exception to represent errors in the genotype data forwarded from the sequence-formats library
    | PoseidonJannoRowException FilePath Int String -- ^ An exception to represent errors when trying to parse the .janno file
    | PoseidonJannoConsistencyException FilePath String -- ^ An exception to represent within-janno consistency errors
    | PoseidonCrossFileConsistencyException String String -- ^ An exception to represent inconsistencies across multiple files in a package
    | PoseidonCollectionException String -- ^ An exception to represent logical issues in a poseidon package Collection
    | PoseidonFileExistenceException FilePath -- ^ An exception to represent missing files
    | PoseidonFileChecksumException FilePath -- ^ An exception to represent failed checksum tests
    | PoseidonFStatsFormatException String -- ^ An exception type to represent FStat specification errors
    | PoseidonBibTeXException FilePath String -- ^ An exception to represent errors when trying to parse the .bib file
    | PoseidonPoseidonEntityParsingException String -- ^ An exception to indicate failed entity parsing
    | PoseidonForgeEntitiesException String -- ^ An exception to indicate issues in the forge selection
    | PoseidonEmptyForgeException -- ^ An exception to throw if there is nothing to be forged
    | PoseidonNewPackageConstructionException String -- ^ An exception to indicate an issue in newPackageTemplate
    | PoseidonRemoteJSONParsingException String -- ^ An exception to indicate failed remote info JSON parsing
    | PoseidonGenericException String -- ^ A catch-all for any other type of exception
    | PoseidonEmptyOutPacNameException -- ^ An exception to throw if the output package lacks a name
    | PoseidonUnequalBaseDirException FilePath FilePath FilePath -- ^ An exception to throw if genotype data files don't share a common base directory
    deriving (Show)

instance Exception PoseidonException

renderPoseidonException :: PoseidonException -> String
renderPoseidonException (PoseidonYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e
renderPoseidonException (PoseidonPackageException s) =
    "Encountered a logical error with a poseidon package: " ++ s
renderPoseidonException (PoseidonPackageVersionException p s) =
    "Poseidon version mismatch in " ++ show p ++
    ". It has version \"" ++ s ++ "\", which is not supported by this trident version."
renderPoseidonException (PoseidonPackageMissingVersionException p) =
    "The POSEIDON.yml file " ++ show p ++ " has no poseidonVersion field. " ++
    "This is mandatory."
renderPoseidonException (PoseidonIndSearchException s) =
    show s
renderPoseidonException (PoseidonGenotypeException s) =
    "Genotype data structurally inconsistent: " ++ show s
renderPoseidonException (PoseidonGenotypeExceptionForward e) =
    "Issues in genotype data parsing: " ++ show e
renderPoseidonException (PoseidonJannoRowException f i s) =
    "Can't read sample in " ++ f ++ " in line " ++ show i ++ ": " ++ s
renderPoseidonException (PoseidonJannoConsistencyException f s) =
    "Consistency issues in .janno file " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonCrossFileConsistencyException p s) =
    "Cross-file consistency issue in package " ++ p ++ ": " ++ s
renderPoseidonException (PoseidonCollectionException s) =
    "The package collection is broken: " ++ s
renderPoseidonException (PoseidonFileExistenceException f) =
    "File " ++ f ++ " does not exist"
renderPoseidonException (PoseidonFileChecksumException f) =
    "File checksum test failed: " ++ f
renderPoseidonException (PoseidonFStatsFormatException s) =
    "Fstat specification error: " ++ s
renderPoseidonException (PoseidonBibTeXException f s) =
    "BibTex problem in file " ++ f ++ ": " ++ s
renderPoseidonException (PoseidonPoseidonEntityParsingException s) =
    "Error when parsing the forge selection: " ++ s
renderPoseidonException (PoseidonForgeEntitiesException s) =
    "Error in the forge selection: " ++ s
renderPoseidonException PoseidonEmptyForgeException =
    "Nothing to be forged"
renderPoseidonException (PoseidonNewPackageConstructionException s) =
    show s
renderPoseidonException (PoseidonRemoteJSONParsingException s) =
    "Error in parsing JSON: " ++ show s
renderPoseidonException (PoseidonGenericException s) = s
renderPoseidonException PoseidonEmptyOutPacNameException =
    "Error when preparing the new package: The output package does not have a name. Add one with: -n YourPackageName"
renderPoseidonException (PoseidonUnequalBaseDirException g s i) =
    "The base directories of these genotype files are not equal."
    ++ " --genoFile: " ++ g
    ++ " --snpFile: "  ++ s
    ++ " --indFile: "  ++ i

-- helper function to check if a file exists
checkFile :: FilePath -> Maybe String -> IO ()
checkFile fn maybeChkSum = do
    fe <- doesFileExist fn
    if not fe
    then throwM (PoseidonFileExistenceException fn)
    else
        case maybeChkSum of
            Nothing -> return ()
            Just chkSum -> do
                fnChkSum <- getChecksum fn
                when (fnChkSum /= chkSum) $ throwM (PoseidonFileChecksumException fn)

-- helper functions to get the checksum of a file
getChecksum :: FilePath -> IO String
getChecksum f = do
    fileContent <- LB.readFile f
    let md5Digest = md5 fileContent
    return $ show md5Digest

-- helper functions to pad and cut strings
padRight :: Int -> String -> String
padRight n s
    | length s >= n = take n s
    | length s < n = s ++ replicate (n - length s) ' '
    | otherwise    = s

padLeft :: Int -> String -> String
padLeft n s
    | length s >= n = reverse (take n (reverse s))
    | length s < n = replicate (n - length s) ' ' ++ s
    | otherwise    = s
