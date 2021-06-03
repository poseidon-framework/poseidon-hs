{-# LANGUAGE OverloadedStrings #-}
module Poseidon.BibFile (readBibTeXFile, writeBibTeXFile, BibTeX, Reference(..)) where

import           Poseidon.Utils    (PoseidonException (..))

import           Control.Exception (throwIO)
import           Control.Monad     (forM_)
import           Data.Char         (isSpace)
import Data.List (intercalate)
import System.IO (hPutStr, withFile, IOMode(..), hPutStrLn)
import           Text.Parsec       (many, many1, sepBy1, try, (<|>), manyTill)
import           Text.Parsec.Char  (char, newline, satisfy, spaces, alphaNum, string, letter, anyChar)
import           Text.Parsec.Text  (Parser, parseFromFile)

data Reference = Reference
    { _bibIdent :: String
    , _bibId   :: String
    , _bibProperties :: [BibProperty]
    }
    deriving (Show, Eq)

data BibProperty = BibProperty {
    _bibPropKey :: String,
    _bibPropVal :: String
} deriving (Show, Eq)

type BibTeX = [Reference]

readBibTeXFile :: FilePath -> IO [Reference]
readBibTeXFile bibPath = do
    res <- parseFromFile bibTeXParser bibPath
    case res of
        Left err  -> throwIO $ PoseidonBibTeXException bibPath (show err)
        Right res_ -> return res_

writeBibTeXFile :: FilePath -> [Reference] -> IO ()
writeBibTeXFile path references = withFile path WriteMode $ \outH -> do
    forM_ references $ \r -> do
        hPutStrLn outH ("@" ++ _bibIdent r ++ "{" ++ _bibId r ++ ",")
        let propLines = ["  " ++ _bibPropKey prop ++ " = " ++ _bibPropVal prop | prop <- _bibProperties r]
        hPutStr outH $ intercalate ",\n" propLines
        hPutStrLn outH "\n}\n"


bibTeXParser :: Parser BibTeX
bibTeXParser = many1 referenceParser

referenceParser :: Parser Reference
referenceParser = do
    _ <- char '@'
    ident <- many1 letter
    _ <- char '{'
    bibkey <- many1 (satisfy (\c -> c /= ',' && not (isSpace c)))
    _ <- char ','
    _ <- string "\n  "
    body <- sepBy1 parseBibProperty (string ",\n  ")
    _ <- newline
    _ <- string "}"
    _ <- spaces
    return $ Reference ident bibkey body

parseBibProperty :: Parser BibProperty
parseBibProperty = do
    key <- many1 letter
    _ <- spaces
    _ <- char '='
    _ <- spaces
    val <- try parseQuotedVal <|> try parseCurelyBracketedVal <|> parseNonQuotedVal
    return $ BibProperty key val
  where
    parseQuotedVal = do
        o <- char '"'
        -- this looks complicated because it needs to allow for the presence of escaped quotes \"
        middle <- many (try (string "\\\"") <|> ((\a -> [a]) <$> satisfy (/= '"')))
        c <- char '"'
        return ([o] ++ concat middle ++ [c])
    parseCurelyBracketedVal = do
        o <- char '{'
        middle <- manyTill anyChar newline
        return ([o] ++ middle)
    parseNonQuotedVal = many1 alphaNum
