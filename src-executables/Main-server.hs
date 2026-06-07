{-# LANGUAGE OverloadedStrings #-}

import           Paths_poseidon_hs                               (version)
import           Poseidon.CLI.Trident.OptparseApplicativeParsers
import           Poseidon.CLI.Trident.Serve                      (ServeOptions (..),
                                                                  runServerMainThread)
import           Poseidon.Core.PoseidonVersion                   (showPoseidonVersion,
                                                                  validPoseidonVersions)
import           Poseidon.Core.Utils                             (ErrorLength (..),
                                                                  LogMode (..),
                                                                  PlinkPopNameMode (..),
                                                                  PoseidonException (..),
                                                                  TestMode,
                                                                  logError,
                                                                  renderPoseidonException,
                                                                  usePoseidonLogger)

import           Control.Applicative                             ((<|>))
import           Control.Exception                               (catch)
import           Data.List                                       (intercalate)
import           Data.Version                                    (showVersion)
import qualified Options.Applicative                             as OP
import           System.Exit                                     (exitFailure)
import           System.IO                                       (hPutStrLn,
                                                                  stderr)

data Options = Options
  { _logMode      :: LogMode
  , _testMode     :: TestMode
  , _errLength    :: ErrorLength
  , _plinkMode    :: PlinkPopNameMode
  , _serveOptions :: ServeOptions
  }

main :: IO ()
main = do
    hPutStrLn stderr renderVersion
    hPutStrLn stderr ""
    Options logMode testMode errLength plinkMode serveOptions <-
        OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    catch (usePoseidonLogger logMode testMode plinkMode errLength $ runServerMainThread serveOptions)
        (handler logMode testMode plinkMode errLength)
  where
    handler :: LogMode -> TestMode -> PlinkPopNameMode -> ErrorLength -> PoseidonException -> IO ()
    handler l t pm len e = do
        usePoseidonLogger l t pm len $ logError $ renderPoseidonException e
        exitFailure

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info
    (OP.helper <*> versionOption <*> optionsParser)
    ( OP.briefDesc <> OP.progDesc "Poseidon webserver")

optionsParser :: OP.Parser Options
optionsParser =
    Options
        <$> (parseLogMode <|> parseDebugMode)
        <*> parseTestMode
        <*> parseErrorLength
        <*> parseInputPlinkPopMode
        <*> serveOptParser

versionOption :: OP.Parser (a -> a)
versionOption =
    OP.infoOption
        (showVersion version)
        (OP.long "version" <> OP.help "Show version number")

renderVersion :: String
renderVersion =
    "poseidon-server v" ++ showVersion version ++ " for poseidon v" ++
    intercalate ", v" (map showPoseidonVersion validPoseidonVersions) ++ "\n" ++
    "https://poseidon-framework.github.io"

serveOptParser :: OP.Parser ServeOptions
serveOptParser =
    ServeOptions
        <$> parseArchiveConfig
        <*> parsePort
        <*> parseIgnoreChecksums
        <*> parseMaybeCertFiles
