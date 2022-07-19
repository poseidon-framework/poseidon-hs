{-# LANGUAGE OverloadedStrings, FlexibleInstances, MultiParamTypeClasses #-}

module Poseidon.Utils (
    PoseidonException (..),
    renderPoseidonException,
    usePoseidonLogger,
    PoseidonLogIO,
    LogMode (..),
    logWarning,
    logInfo,
    logDebug,
    logError,
    LogEnv,
    noLog
) where

import           Colog                  (LogAction (..), Message,
                                         Severity (..), cfilter, cmapM,
                                         logTextStderr, msgSeverity, msgText,
                                         showSeverity, Msg(..), HasLog(..))
import           Control.Exception      (Exception, IOException, try)
import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, runReaderT, asks)
import           Data.Text              (Text, pack)
import           Data.Time              (defaultTimeLocale, formatTime,
                                         getCurrentTime, utcToLocalZonedTime)
import           Data.Yaml              (ParseException)
import           GHC.Stack              (callStack, withFrozenCallStack)
import           System.Console.ANSI    (getCursorPosition)
import           System.IO              (hPutStrLn, stderr)

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

noLog      :: LogEnv
noLog      = cfilter (const False) simpleLog
simpleLog  :: LogEnv
simpleLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg False False True
defaultLog :: LogEnv
defaultLog = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True False True
serverLog  :: LogEnv
serverLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True True True
verboseLog :: LogEnv
verboseLog = compileLogMsg True True True

compileLogMsg :: Bool -> Bool -> Bool -> LogEnv
compileLogMsg severity time cursorCheck = cmapM prepareMessage logTextStderr
    where
        prepareMessage :: Message -> IO Text
        prepareMessage msg = do
            -- add a newline, if the current line is not empty (to handle e.g. the SNP or Pac progress counter)
            when cursorCheck $ do
                eitherCurserPos <- try getCursorPosition :: IO (Either IOException (Maybe (Int, Int)))
                case eitherCurserPos of
                    Left _ -> return ()
                    Right cursorPos -> do
                        let col = maybe 0 snd cursorPos
                            isNotAtStartOfLine = col /= 0
                        when isNotAtStartOfLine $ do hPutStrLn stderr ""
            -- prepare message
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

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException = PoseidonYamlParseException FilePath ParseException
    | PoseidonPackageException String
    | PoseidonPackageVersionException FilePath String
    | PoseidonPackageMissingVersionException FilePath
    | PoseidonIndSearchException String
    | PoseidonGenotypeException String
    | PoseidonJannoRowException FilePath Int String
    | PoseidonJannoConsistencyException FilePath String
    | PoseidonCrossFileConsistencyException String String
    | PoseidonCollectionException String
    | PoseidonFileExistenceException FilePath
    | PoseidonFileChecksumException FilePath
    | PoseidonFStatsFormatException String
    | PoseidonBibTeXException FilePath String
    | PoseidonPoseidonEntityParsingException String
    | PoseidonForgeEntitiesException String
    | PoseidonEmptyForgeException
    | PoseidonNewPackageConstructionException String
    | PoseidonRemoteJSONParsingException String
    | PoseidonGenericException String
    | PoseidonEmptyOutPacNameException
    | PoseidonUnequalBaseDirException FilePath FilePath FilePath
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
    "Error in the genotype data: " ++ show s
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
