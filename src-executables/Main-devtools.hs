{-# LANGUAGE OverloadedStrings #-}

import           Paths_poseidon_hs                  (version)
import           Poseidon.GoldenTestsRunCommands    (createStaticCheckSumFile)

import           Control.Applicative                ((<|>))
import           Control.Exception                  (catch)
import           Data.Version                       (showVersion)
import qualified Options.Applicative                as OP
import           System.Exit                        (exitFailure)
import           System.IO                          (hPutStrLn, stderr)

data UpdateGoldenTestsOptions = UpdateGoldenTestsOptions
    { _optPoseidonHSDir :: FilePath
    }

data Options = CmdUpdateGoldenTests UpdateGoldenTestsOptions

main :: IO ()
main = do
    cmdOpts <- OP.customExecParser p optParserInfo
    runCmd cmdOpts
    where
        p = OP.prefs OP.showHelpOnEmpty

runCmd :: Options -> IO ()
runCmd o = case o of
    CmdUpdateGoldenTests opts -> runUpdateGoldenTests opts

optParserInfo :: OP.ParserInfo Options
optParserInfo = OP.info (OP.helper <*> versionOption <*> optParser) (
    OP.briefDesc <>
    OP.progDesc "developer tools for poseidon-hs"
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show version")

optParser :: OP.Parser Options
optParser = OP.subparser (
        OP.command "updateGoldenTests" updateGoldenTestsOptInfo
    )
  where
    updateGoldenTestsOptInfo = OP.info (OP.helper <*> (CmdUpdateGoldenTests <$> testOptParser))
        (OP.progDesc "update the staticChecksumFile in test/testDat by running createStaticCheckSumFile")

testOptParser :: OP.Parser UpdateGoldenTestsOptions
testOptParser = UpdateGoldenTestsOptions <$> parsePoseidonHSDir

parsePoseidonHSDir :: OP.Parser FilePath
parsePoseidonHSDir = OP.strOption (
    OP.long "poseidonHSDir" <>
    OP.short 'd' <>
    OP.help "path to the poseidon-hs directory"
    )

runUpdateGoldenTests :: UpdateGoldenTestsOptions -> IO ()
runUpdateGoldenTests (UpdateGoldenTestsOptions poseidonHSDir) = do
    hPutStrLn stderr "Attempting to run pipeline"
    createStaticCheckSumFile poseidonHSDir
    hPutStrLn stderr "Checksum update successful"
