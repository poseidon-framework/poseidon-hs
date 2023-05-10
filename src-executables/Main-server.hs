{-# LANGUAGE OverloadedStrings #-}

import           Poseidon.CLI.OptparseApplicativeParsers (parseInputPlinkPopMode)
import           Poseidon.CLI.Server                     (CommandLineOptions (..),
                                                          runServerMainThread)
import           Poseidon.Utils                          (LogMode (..),
                                                          usePoseidonLogger)

import           Data.Version                            (showVersion)
import qualified Options.Applicative                     as OP
import           Paths_poseidon_hs                       (version)

main :: IO ()
main = do
    opts <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    usePoseidonLogger VerboseLog (cliPlinkPopMode opts) (runServerMainThread opts)

optParserInfo :: OP.ParserInfo CommandLineOptions
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "poseidon-http-server is a HTTP Server to provide information about \
        \Poseidon package repositories. \
        \More information: \
        \https://github.com/poseidon-framework/poseidon-hs. \
        \Report issues: \
        \https://github.com/poseidon-framework/poseidon-hs/issues")

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser CommandLineOptions
optParser = CommandLineOptions <$> parseBasePaths <*> parseMaybeZipDir <*> parsePort <*>
    parseIgnoreChecksums <*> parseMaybeCertFiles <*> parseInputPlinkPopMode

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages"))

parseMaybeZipDir :: OP.Parser (Maybe FilePath)
parseMaybeZipDir = OP.option (Just <$> OP.str) (OP.long "zipDir" <>
    OP.short 'z' <>
    OP.metavar "DIR" <>
    OP.help "a directory to store Zip files in. If not specified, do not generate zip files" <>
    OP.value Nothing)

parsePort :: OP.Parser Int
parsePort = OP.option OP.auto (OP.long "port" <> OP.short 'p' <> OP.metavar "PORT" <>
    OP.value 3000 <> OP.showDefault <>
    OP.help "the port on which the server listens")

parseIgnoreChecksums :: OP.Parser Bool
parseIgnoreChecksums = OP.switch (OP.long "ignoreChecksums" <> OP.short 'c' <>
    OP.help "whether to ignore checksums. Useful for speedup in debugging")

parseMaybeCertFiles :: OP.Parser (Maybe (FilePath, [FilePath], FilePath))
parseMaybeCertFiles = OP.optional parseFiles
  where
    parseFiles = (,,) <$> parseCertFile <*> OP.many parseChainFile <*> parseKeyFile

parseKeyFile :: OP.Parser FilePath
parseKeyFile = OP.strOption (OP.long "keyFile" <> OP.metavar "KEYFILE" <>
                             OP.help "The key file of the TLS Certificate used for HTTPS")

parseChainFile :: OP.Parser FilePath
parseChainFile = OP.strOption (OP.long "chainFile" <> OP.metavar "CHAINFILE" <>
                               OP.help "The chain file of the TLS Certificate used for HTTPS. Can be given multiple times")

parseCertFile :: OP.Parser FilePath
parseCertFile = OP.strOption (OP.long "certFile" <> OP.metavar "CERTFILE" <>
                              OP.help "The cert file of the TLS Certificate used for HTTPS")
