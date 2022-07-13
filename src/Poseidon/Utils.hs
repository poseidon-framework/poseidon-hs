{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Utils (
    PoseidonException (..),
    renderPoseidonException,
    usePoseidonLogger,
    PoseidonLogIO,
    LogMode (..)
) where

import           Colog               (LogAction (..), LoggerT, Message,
                                      Severity (..), cfilter, cmapM,
                                      logTextStderr, msgSeverity, msgText,
                                      showSeverity, usingLoggerT)
import           Control.Exception   (Exception, IOException, try)
import           Control.Monad       (when)
import           Data.Text           (Text, pack)
import           Data.Time           (defaultTimeLocale, formatTime,
                                      getCurrentTime, utcToLocalZonedTime)
import           Data.Yaml           (ParseException)
import           System.Console.ANSI (getCursorPosition)
import           System.IO           (hPutStrLn, stderr)

type PoseidonLogIO = LoggerT Message IO

data LogMode = NoLog | SimpleLog | DefaultLog | ServerLog | VerboseLog
    deriving Show

usePoseidonLogger :: LogMode -> PoseidonLogIO a -> IO a
usePoseidonLogger NoLog      = usingLoggerT noLog
usePoseidonLogger SimpleLog  = usingLoggerT simpleLog
usePoseidonLogger DefaultLog = usingLoggerT defaultLog
usePoseidonLogger ServerLog  = usingLoggerT serverLog
usePoseidonLogger VerboseLog = usingLoggerT verboseLog

noLog      :: LogAction IO Message
noLog      = cfilter (const False) simpleLog
simpleLog  :: LogAction IO Message
simpleLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg False False True
defaultLog :: LogAction IO Message
defaultLog = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True False True
serverLog  :: LogAction IO Message
serverLog  = cfilter (\msg -> msgSeverity msg /= Debug) $ compileLogMsg True True True
verboseLog :: LogAction IO Message
verboseLog = compileLogMsg True True True

compileLogMsg :: Bool -> Bool -> Bool -> LogAction IO Message
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

-- | A Poseidon Exception data type with several concrete constructors
data PoseidonException =
    PoseidonYamlParseException FilePath ParseException -- ^ An exception to represent YAML parsing errors
    | PoseidonPackageException String -- ^ An exception to represent a logical error in a package
    | PoseidonPackageVersionException FilePath String -- ^ An exception to represent an issue with a package version
    | PoseidonPackageMissingVersionException FilePath -- ^ An exception to indicate a missing poseidonVersion field
    | PoseidonIndSearchException String -- ^ An exception to represent an error when searching for individuals or populations
    | PoseidonGenotypeException String -- ^ An exception to represent errors when trying to parse the genotype data
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
