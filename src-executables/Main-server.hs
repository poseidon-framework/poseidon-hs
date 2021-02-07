{-# LANGUAGE OverloadedStrings #-}

import           Data.Version        (showVersion)
import qualified Options.Applicative as OP
import           Paths_poseidon_hs   (version)
import           Web.Scotty          (get, param, scotty, text)

data CommandLineOptions = CommandLineOptions
    { cliBaseDirs :: [FilePath]
    , cliPort     :: Int
    }
    deriving (Show)

main :: IO ()
main = do
    opts@(CommandLineOptions baseDirs port) <- OP.customExecParser p optParserInfo
    scotty port $ do
        get "/packages" $ do
            text "You requested packages"
        get "/:package_name/info" $ do
            p <- param "package_name"
            text ("You requested info for package " <> p)
        get "/:package_name/zip_file" $ do
            p <- param "package_name"
            text ("You requested the zip_file for package " <> p)
        get "/:package_name/individuals" $ do
            p <- param "package_name"
            text ("You requested the individual entries for package " <> p)
  where
    p = OP.prefs OP.showHelpOnEmpty

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
optParser = CommandLineOptions <$> parseBasePaths <*> parsePort

parseBasePaths :: OP.Parser [FilePath]
parseBasePaths = OP.some (OP.strOption (OP.long "baseDir" <>
    OP.short 'd' <>
    OP.metavar "DIR" <>
    OP.help "a base directory to search for Poseidon Packages (could be a Poseidon repository)"))

parsePort :: OP.Parser Int
parsePort = OP.option OP.auto (OP.long "port" <> OP.short 'p' <> OP.metavar "PORT" <>
    OP.value 3000 <> OP.showDefault <>
    OP.help "the port on which the server listens")
