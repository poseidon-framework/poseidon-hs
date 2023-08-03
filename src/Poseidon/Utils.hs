{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Utils (
    PoseidonException (..),
    renderPoseidonException,
    usePoseidonLogger,
    testLog,
    PoseidonIO,
    envLogAction,
    envInputPlinkMode,
    LogMode (..),
    checkFile,
    getChecksum,
    logWarning,
    logInfo,
    logDebug,
    logError,
    LogA,
    noLog,
    logWithEnv,
    padRight, padLeft,
    determinePackageOutName,
    PlinkPopNameMode(..),
    TestMode(..),
    Env(..),
    uniquePO, uniqueRO
) where

import           Paths_poseidon_hs      (version)

import           Colog                  (HasLog (..), LogAction (..), Message,
                                         Msg (..), Severity (..), cfilter,
                                         cmapM, logTextStderr, msgSeverity,
                                         msgText, showSeverity)
import           Control.Exception      (Exception, throwIO)
import           Control.Exception.Base (SomeException)
import           Control.Monad          (when)
import           Control.Monad.Catch    (throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT, asks, runReaderT)
import qualified Data.ByteString.Lazy   as LB
import           Data.Digest.Pure.MD5   (md5)
import qualified Data.Set               as Set
import           Data.Text              (Text, pack)
import           Data.Time              (defaultTimeLocale, formatTime,
                                         getCurrentTime, utcToLocalZonedTime)
import           Data.Version           (showVersion)
import           Data.Yaml              (ParseException)
import           GHC.Stack              (callStack, withFrozenCallStack)
import           Network.HTTP.Conduit   (HttpException (..))
import           SequenceFormats.Plink  (PlinkPopNameMode (..))
import           System.Directory       (doesFileExist)
import           System.FilePath.Posix  (takeBaseName)

type LogA = LogAction IO Message

data TestMode = Testing | Production deriving Show

data Env = Env {
    _envLogAction      :: LogA,
    _envTestMode       :: TestMode,
    _envInputPlinkMode :: PlinkPopNameMode
}

defaultEnv :: LogA -> Env
defaultEnv logA = Env logA Production PlinkPopNameAsFamily

type PoseidonIO = ReaderT Env IO

-- just two convenience helper functions
envLogAction :: PoseidonIO LogA
envLogAction = asks _envLogAction

envInputPlinkMode :: PoseidonIO PlinkPopNameMode
envInputPlinkMode = asks _envInputPlinkMode

data LogMode = NoLog
    | SimpleLog
    | DefaultLog
    | ServerLog
    | VerboseLog
    deriving Show

usePoseidonLogger :: LogMode -> TestMode -> PlinkPopNameMode -> PoseidonIO a -> IO a
usePoseidonLogger NoLog      testMode plinkMode = flip runReaderT (Env noLog testMode plinkMode)
usePoseidonLogger SimpleLog  testMode plinkMode = flip runReaderT (Env simpleLog testMode plinkMode)
usePoseidonLogger DefaultLog testMode plinkMode = flip runReaderT (Env defaultLog testMode plinkMode)
usePoseidonLogger ServerLog  testMode plinkMode = flip runReaderT (Env serverLog testMode plinkMode)
usePoseidonLogger VerboseLog testMode plinkMode = flip runReaderT (Env verboseLog testMode plinkMode)

testLog :: PoseidonIO a -> IO a
testLog = usePoseidonLogger NoLog Testing PlinkPopNameAsFamily
--testLog = usePoseidonLogger VerboseLog Testing PlinkPopNameAsFamily

noLog      :: LogA
noLog      = cfilter (const False) simpleLog
simpleLog  :: LogA
simpleLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg False False
defaultLog :: LogA
defaultLog = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True False
serverLog  :: LogA
serverLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True True
verboseLog :: LogA
verboseLog = compileLogMsg True True

compileLogMsg :: Bool -> Bool -> LogA
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
                        return $ pack $ "[" ++ formatTime defaultTimeLocale "%F %T" zonedTime ++ "] "
                    else mempty
            return $ textSeverity <> textTime <> textMessage

logMsg :: Severity -> String -> PoseidonIO ()
logMsg sev msg = do
    {-
    Using asks getLogAction here gives us a bit of flexibility. If in the future we'd like to expand the
    ReaderT environment by adding more options or parameters to LogA, perhaps even the command line options,
    we can do so, we just need to adapt the HasLog instance, which tells us how to get the logAction out of the
    environment.
    -}
    LogAction logF <- asks (getLogAction . _envLogAction)
    liftIO . withFrozenCallStack . logF $ Msg sev callStack (pack msg)

logWarning :: String -> PoseidonIO ()
logWarning = logMsg Warning

logInfo :: String -> PoseidonIO ()
logInfo = logMsg Info

logDebug :: String -> PoseidonIO ()
logDebug = logMsg Debug

logError :: String -> PoseidonIO ()
logError = logMsg Error

logWithEnv :: (MonadIO m) => LogA -> PoseidonIO () -> m ()
logWithEnv logA = liftIO . flip runReaderT (defaultEnv logA)

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException =
      PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonPackageVersionException FilePath String -- ^ An exception to represent an issue with a package version
    | PoseidonPackageMissingVersionException FilePath -- ^ An exception to indicate a missing poseidonVersion field
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors in the genotype data
    | PoseidonGenotypeExceptionForward SomeException -- ^ An exception to represent errors in the genotype data forwarded from the sequence-formats library
    | PoseidonHttpExceptionForward HttpException -- ^ An exception to represent errors in the remote data loading forwarded from simpleHttp
    | PoseidonFileRowException FilePath Int String -- ^ An exception to represent errors when trying to parse the janno or seqSource file
    | PoseidonFileConsistencyException FilePath String -- ^ An exception to represent consistency errors in janno or seqSource files
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
    | PoseidonServerCommunicationException String -- ^ An exception to mark server communication errors
    | PoseidonUnzipException SomeException -- ^ An exception for unzipping issues in fetch
    | PoseidonChronicleException String -- ^ An exception for issues in chronicle
    | PoseidonGitException FilePath String -- ^ An exception for issues with git
    deriving (Show)

instance Exception PoseidonException

renderPoseidonException :: PoseidonException -> String
renderPoseidonException (PoseidonYamlParseException fn e) =
    "Could not parse YAML file " ++ fn ++ ": " ++ show e
renderPoseidonException (PoseidonPackageException s) =
    "Encountered a logical error with a poseidon package: " ++ s
renderPoseidonException (PoseidonPackageVersionException p s) =
    "Poseidon version mismatch in " ++ show p ++
    ". This package is build according to poseidon schema v" ++ s ++
    ", which is not supported by trident v" ++ showVersion version ++
    ". Modify the package, or download a newer (or older) version of trident."
renderPoseidonException (PoseidonPackageMissingVersionException p) =
    "The POSEIDON.yml file " ++ show p ++ " has no poseidonVersion field. " ++
    "This is mandatory."
renderPoseidonException (PoseidonIndSearchException s) =
    show s
renderPoseidonException (PoseidonGenotypeException s) =
    "Genotype data structurally inconsistent: " ++ show s
renderPoseidonException (PoseidonGenotypeExceptionForward e) =
    "Issues in genotype data parsing: " ++ show e
renderPoseidonException (PoseidonHttpExceptionForward (HttpExceptionRequest _ content)) =
    "Issues in HTTP-communication with server:\n" ++
    show content
renderPoseidonException (PoseidonHttpExceptionForward e) =
    "Issues in HTTP-communication with server: " ++ show e
renderPoseidonException (PoseidonFileRowException f i s) =
    "Can't read sample in " ++ f ++ " in line " ++ show i ++ ": " ++ s
renderPoseidonException (PoseidonFileConsistencyException f s) =
    "Consistency issues in file " ++ f ++ ": " ++ s
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
renderPoseidonException (PoseidonServerCommunicationException e) = e
renderPoseidonException (PoseidonUnzipException e) =
    "Error during unzipping: " ++ show e
renderPoseidonException (PoseidonChronicleException s) =
    "Error when preparing the chronicle file: "  ++ s
renderPoseidonException (PoseidonGitException p s) =
    "Failed to load .git directory in " ++ p ++ ": " ++ s

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

-- helper function to determine the package name depending on -n and -o
determinePackageOutName :: Maybe String -> FilePath -> IO String
determinePackageOutName maybeOutName outPath = do
    case maybeOutName of -- take basename of outPath, if name is not provided
            Just x  -> return x
            Nothing -> case takeBaseName outPath of -- check if outPath is empty
                "" -> throwIO PoseidonEmptyOutPacNameException
                y  -> return y

-- two helper functions to reduce a lists to the unique elements in it
-- see https://github.com/nh2/haskell-ordnub#dont-use-nub
-- preserves the original order
uniquePO :: (Ord a) => [a] -> [a]
uniquePO = go Set.empty
  where
    go _ [] = []
    go s (x:xs) =
        if x `Set.member` s
        then go s xs
        else x : go (Set.insert x s) xs

-- reorderes the list according to the Ord instance of a
uniqueRO :: (Ord a) => [a] -> [a]
uniqueRO = Set.toList . Set.fromList
