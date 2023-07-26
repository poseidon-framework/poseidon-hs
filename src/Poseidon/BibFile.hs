{-# LANGUAGE OverloadedStrings #-}
module Poseidon.BibFile (dummyBibEntry, readBibTeXFile, writeBibTeXFile, BibTeX, BibEntry(..)) where

import           Poseidon.Utils                     (PoseidonException (..))

import           Control.Exception                  (throwIO)
import           Control.Monad                      (forM_, liftM2, liftM3)
import           System.IO                          (IOMode (..), hPutStrLn,
                                                     withFile)
import           Text.Parsec                        (between, char, many, many1,
                                                     noneOf, oneOf, sepEndBy,
                                                     try, (<|>))
import           Text.Parsec.Char                   (alphaNum, digit, letter)
import           Text.Parsec.Language               (emptyDef)
import           Text.Parsec.String                 (Parser, parseFromFile)
import qualified Text.Parsec.Token                  as T
import           Text.ParserCombinators.Parsec.Char (CharParser)

data BibEntry = BibEntry
    { bibEntryType   :: String
    , bibEntryId     :: String
    , bibEntryFields :: [(String, String)]
    }
    deriving (Show)

instance Eq BibEntry where
    (BibEntry t1 i1 _) == (BibEntry t2 i2 _) = (t1 == t2) && (i1 == i2)

instance Ord BibEntry where
    (BibEntry _ i1 _) `compare` (BibEntry _ i2 _) = i1 `compare` i2

type BibTeX = [BibEntry]

dummyBibEntry :: BibEntry
dummyBibEntry = BibEntry
    { bibEntryType   = "article"
    , bibEntryId     = "exampleBibtexKey"
    , bibEntryFields = [
       ("title", "Example Paper"),
       ("author", "Doe, John"),
       ("year", "2018"),
       ("journal", "Example Journal"),
       ("volume", "47"),
       ("issue", "10"),
       ("publisher", "The example society for example research"),
       ("doi","10.XXXX/ExampleJournal.47.777"),
       ("url","https://doi.org/10.XXXX/ExampleJournal.47.777")
      ]
    }

readBibTeXFile :: FilePath -> IO BibTeX
readBibTeXFile bibPath = do
    res <- parseFromFile bibFileParser bibPath
    case res of
        Left err   -> throwIO $ PoseidonBibTeXException bibPath (show err)
        Right res_ -> return res_

{-
Much of the code below was shamelessly copied from the existing Haskell package "bibtex"
by Henning Thielemann. The package seems to be dead, and I needed to make some changes. So I
copied the relevant code here and modified it as needed.
-}

writeBibTeXFile :: FilePath -> BibTeX -> IO ()
writeBibTeXFile path entries = withFile path WriteMode $ \outH -> do
    forM_ entries $ \bibEntry -> do
        let entryString = writeEntry bibEntry
        hPutStrLn outH entryString
  where
    writeEntry :: BibEntry -> String
    writeEntry (BibEntry entryType bibId items) =
        let formatItem (name, value_) =
                "  " ++ name ++ " = {" ++ value_ ++ "},\n"
        in  "@" ++ entryType ++ "{" ++ bibId ++ ",\n" ++
            concatMap formatItem items ++ "}\n"

bibFileParser :: Parser [BibEntry]
bibFileParser = bibCommentParser >> sepEndBy bibEntryParser bibCommentParser

bibCommentParser :: Parser String
bibCommentParser = many $ noneOf "@"

bibEntryParser :: Parser BibEntry
bibEntryParser =
   do entryType <- char '@' >> identifier
      braces $
         liftM2 (BibEntry entryType)
            (try bibIdentifier)
            (comma >> sepEndBy assignment comma)

identifier :: CharParser st String
identifier = T.identifier lexer

lexer :: T.TokenParser st
lexer =
   T.makeTokenParser $ emptyDef {
      T.commentLine = "%",
      T.identStart = alphaNum,
      T.identLetter = alphaNum
   }

braces :: CharParser st a -> CharParser st a
braces = T.braces lexer

bibIdentifier :: Parser String
bibIdentifier = lexeme $
   liftM2 (:) (alphaNum <|> char '_') (many (alphaNum <|> oneOf "&;:-_.?+/"))

lexeme :: CharParser st a -> CharParser st a
lexeme = T.lexeme lexer

assignment :: Parser (String, String)
assignment =
   liftM2 (,)
      bibIdentifier
      (equals >> value)

equals :: CharParser st String
equals = T.symbol lexer "="

value :: Parser String
value =
   lexeme (many1 letter) <|> -- for fields like: month = jul
   lexeme (many1 digit)  <|> -- for fields like: year = 2010
   braces (texSequence '}') <|>
   lexeme (between (char '"') (char '"') (texSequence '"'))

texSequence :: Char -> Parser String
texSequence closeChar =
   concat <$> many (texBlock closeChar)

texBlock :: Char -> Parser String
texBlock closeChar =
   liftM3 (\open body close -> open : body ++ [close])
      (char '{') (texSequence '}') (char '}') <|>
   sequence
      [char '\\',
       oneOf "=\\_{}[]$|'`^&%\".,~# " <|> letter] <|>
   fmap (:[]) (noneOf [closeChar])


comma :: CharParser st String
comma = T.comma lexer

