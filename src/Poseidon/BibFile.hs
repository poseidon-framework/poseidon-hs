{-# LANGUAGE OverloadedStrings #-}
module Poseidon.BibFile (readBibTeXFile, writeBibTeXFile, BibTeX, Reference(..)) where

import           Poseidon.Utils    (PoseidonException (..))

import           Control.Exception (catch, throwIO)
import           Control.Monad     (when, forM_)
import           Data.Char         (isSpace)
import qualified Data.Text         as T
import qualified Data.Text.IO      as Tio
import           System.Directory  (doesFileExist)
import System.IO (hPutStr, withFile, IOMode(..), hPutStrLn)
import           Text.Parsec       (many, many1, sepBy1, try, (<|>))
import           Text.Parsec.Char  (char, newline, satisfy, spaces, upper, lower, alphaNum, string, letter)
import           Text.Parsec.Text  (Parser, parseFromFile)

-- import           Text.CSL                (procOpts, processBibliography,
--                                           renderPlain, parseCSL)
-- import           Text.CSL.Style          (Style (..))
-- import           Text.CSL.Exception      (CiteprocException)
-- import           Text.CSL.Input.Bibtex   (readBibtex)
-- import           Text.CSL.Reference      (Reference (..))

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
        Right res -> return res

writeBibTeXFile :: FilePath -> [Reference] -> IO ()
writeBibTeXFile path references = withFile path WriteMode $ \outH -> do
    forM_ references $ \r -> do
        hPutStrLn outH ("@" ++ _bibIdent r ++ "{" ++ _bibId r ++ ",")
        forM_ (_bibProperties r) $ \prop -> do
            hPutStr outH "  "
            hPutStr outH (_bibPropKey prop)
            hPutStr outH " = "
            hPutStr outH (_bibPropVal prop)
            hPutStrLn outH ","
        hPutStrLn outH ""


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
    key <- many1 lower
    _ <- spaces
    _ <- char '='
    _ <- spaces
    val <- try parseQuotedVal <|> parseNonQuotedVal
    return $ BibProperty key val
  where
    parseQuotedVal = do
        o <- char '"'
        middle <- many1 (satisfy (/= '"'))
        c <- char '"'
        return ([o] ++ middle ++ [c])
    parseNonQuotedVal = many1 alphaNum




-- readBibTeXFile :: FilePath -> IO [Reference]
-- readBibTeXFile bibPath = do
--     fileE <- doesFileExist bibPath
--     when (not fileE) . throwIO $ PoseidonFileExistenceException ("Could not find bib file " ++ bibPath)
--     catch (readBibtex (const True) True False bibPath) handler
--   where
--     handler :: CiteprocException -> IO [Reference]
--     handler e = throwIO $ PoseidonBibTeXException bibPath (show e)

-- writeBibTeXFile ::  FilePath -> [Reference] -> IO()
-- writeBibTeXFile path references_ = do
--     let renderedReferences = processBibliography procOpts bibTeXCSLStyle references_
--     let referencesTexts = map renderPlain renderedReferences
--     let referencesTextsFixed = map cleanBibTeXString referencesTexts
--     let huup = T.intercalate "\n\n" referencesTextsFixed
--     Tio.writeFile path huup

-- cleanBibTeXString :: T.Text -> T.Text
-- cleanBibTeXString =
--     T.replace "} }" "}\n}"
--     . T.replace ", title=" ",\n  title="
--     . T.replace "   " "  "
--     . T.replace "}," "},\n  "
--     . T.replace "\n" " "

-- bibTeXCSLStyle :: Style
-- bibTeXCSLStyle = parseCSL "\
--     \ <?xml version=\"1.0\" encoding=\"utf-8\"?> \
--     \ <style xmlns=\"http://purl.org/net/xbiblio/csl\" class=\"in-text\" version=\"1.0\" demote-non-dropping-particle=\"sort-only\" default-locale=\"en-US\"> \
--     \ <info> \
--         \ <title>BibTeX generic citation style</title> \
--         \ <id>http://www.zotero.org/styles/bibtex</id> \
--         \ <link href=\"http://www.zotero.org/styles/bibtex\" rel=\"self\"/> \
--         \ <link href=\"http://www.bibtex.org/\" rel=\"documentation\"/> \
--         \ <author> \
--         \ <name>Markus Schaffner</name> \
--         \ </author> \
--         \ <contributor> \
--         \ <name>Richard Karnesky</name> \
--         \ <email>karnesky+zotero@gmail.com</email> \
--         \ <uri>http://arc.nucapt.northwestern.edu/Richard_Karnesky</uri> \
--         \ </contributor> \
--         \ <category citation-format=\"author-date\"/> \
--         \ <category field=\"generic-base\"/> \
--         \ <updated>2012-09-14T21:22:32+00:00</updated> \
--         \ <rights license=\"http://creativecommons.org/licenses/by-sa/3.0/\">This work is licensed under a Creative Commons Attribution-ShareAlike 3.0 License</rights> \
--     \ </info> \
--     \ <macro name=\"zotero2bibtexType\"> \
--         \ <choose> \
--         \ <if type=\"bill book graphic legal_case legislation motion_picture report song\" match=\"any\"> \
--             \ <text value=\"book\"/> \
--         \ </if> \
--         \ <else-if type=\"chapter\" match=\"any\"> \
--             \ <text value=\"inbook\"/> \
--         \ </else-if> \
--         \ <else-if type=\"article article-journal article-magazine article-newspaper\" match=\"any\"> \
--             \ <text value=\"article\"/> \
--         \ </else-if> \
--         \ <else-if type=\"thesis\" match=\"any\"> \
--             \ <text value=\"phdthesis\"/> \
--         \ </else-if> \
--         \ <else-if type=\"manuscript\" match=\"any\"> \
--             \ <text value=\"unpublished\"/> \
--         \ </else-if> \
--         \ <else-if type=\"paper-conference\" match=\"any\"> \
--             \ <text value=\"inproceedings\"/> \
--         \ </else-if> \
--         \ <else-if type=\"report\" match=\"any\"> \
--             \ <text value=\"techreport\"/> \
--         \ </else-if> \
--         \ <else> \
--             \ <text value=\"misc\"/> \
--         \ </else> \
--         \ </choose> \
--     \ </macro> \
--     \ <!-- <macro name=\"citeKey\"> \
--         \ <group delimiter=\"_\"> \
--         \ <text macro=\"author-short\" text-case=\"lowercase\"/> \
--         \ <text macro=\"issued-year\"/> \
--         \ </group> \
--     \ </macro> --> \
--     \ <macro name=\"author-short\"> \
--         \ <names variable=\"author\"> \
--         \ <name form=\"short\" delimiter=\"_\" delimiter-precedes-last=\"always\"/> \
--         \ <substitute> \
--             \ <names variable=\"editor\"/> \
--             \ <names variable=\"translator\"/> \
--             \ <choose> \
--             \ <if type=\"bill book graphic legal_case legislation motion_picture report song\" match=\"any\"> \
--                 \ <text variable=\"title\" form=\"short\"/> \
--             \ </if> \
--             \ <else> \
--                 \ <text variable=\"title\" form=\"short\"/> \
--             \ </else> \
--             \ </choose> \
--         \ </substitute> \
--         \ </names> \
--     \ </macro> \
--     \ <macro name=\"issued-year\"> \
--         \ <date variable=\"issued\"> \
--         \ <date-part name=\"year\"/> \
--         \ </date> \
--     \ </macro> \
--     \ <macro name=\"issued-month\"> \
--         \ <date variable=\"issued\"> \
--         \ <date-part name=\"month\" form=\"short\" strip-periods=\"true\"/> \
--         \ </date> \
--     \ </macro> \
--     \ <macro name=\"author\"> \
--         \ <names variable=\"author\"> \
--         \ <name sort-separator=\", \" delimiter=\" and \" delimiter-precedes-last=\"always\" name-as-sort-order=\"all\"/> \
--         \ </names> \
--     \ </macro> \
--     \ <macro name=\"editor-translator\"> \
--         \ <names variable=\"editor translator\" delimiter=\", \"> \
--         \ <name sort-separator=\", \" delimiter=\" and \" delimiter-precedes-last=\"always\" name-as-sort-order=\"all\"/> \
--         \ </names> \
--     \ </macro> \
--     \ <macro name=\"title\"> \
--         \ <text variable=\"title\"/> \
--     \ </macro> \
--     \ <macro name=\"number\"> \
--         \ <text variable=\"issue\"/> \
--         \ <text variable=\"number\"/> \
--     \ </macro> \
--     \ <macro name=\"container-title\"> \
--         \ <choose> \
--         \ <if type=\"chapter paper-conference\" match=\"any\"> \
--             \ <text variable=\"container-title\" prefix=\" booktitle={\" suffix=\"}\"/> \
--         \ </if> \
--         \ <else> \
--             \ <text variable=\"container-title\" prefix=\" journal={\" suffix=\"}\"/> \
--         \ </else> \
--         \ </choose> \
--     \ </macro> \
--     \ <macro name=\"publisher\"> \
--         \ <choose> \
--         \ <if type=\"thesis\"> \
--             \ <text variable=\"publisher\" prefix=\" school={\" suffix=\"}\"/> \
--         \ </if> \
--         \ <else-if type=\"report\"> \
--             \ <text variable=\"publisher\" prefix=\" institution={\" suffix=\"}\"/> \
--         \ </else-if> \
--         \ <else> \
--             \ <text variable=\"publisher\" prefix=\" publisher={\" suffix=\"}\"/> \
--         \ </else> \
--         \ </choose> \
--     \ </macro> \
--     \ <macro name=\"pages\"> \
--         \ <text variable=\"page\"/> \
--     \ </macro> \
--     \ <macro name=\"edition\"> \
--         \ <text variable=\"edition\"/> \
--     \ </macro> \
--     \ <!-- <citation et-al-min=\"11\" et-al-use-first=\"10\" disambiguate-add-year-suffix=\"true\" disambiguate-add-names=\"false\" disambiguate-add-givenname=\"false\" collapse=\"year\"> \
--         \ <sort> \
--         \ <key macro=\"author\"/> \
--         \ <key variable=\"issued\"/> \
--         \ </sort> \
--         \ <layout delimiter=\"_\"> \
--         \ <text macro=\"citeKey\"/> \
--         \ </layout> \
--     \ </citation> --> \
--     \ <bibliography hanging-indent=\"false\" et-al-min=\"100\" et-al-use-first=\"100\"> \
--         \ <sort> \
--         \ <key macro=\"author\"/> \
--         \ <key variable=\"issued\"/> \
--         \ </sort> \
--         \ <layout> \
--         \ <text macro=\"zotero2bibtexType\" prefix=\" @\"/> \
--         \ <group prefix=\"{\" suffix=\" }\" delimiter=\", \"> \
--             \ <!-- <text macro=\"citeKey\"/> --> \
--             \ <text variable=\"ref-id\"/> \
--             \ <text variable=\"publisher-place\" prefix=\" place={\" suffix=\"}\"/> \
--             \ <!--Fix This--> \
--             \ <text variable=\"chapter-number\" prefix=\" chapter={\" suffix=\"}\"/> \
--             \ <!--Fix This--> \
--             \ <text macro=\"edition\" prefix=\" edition={\" suffix=\"}\"/> \
--             \ <!--Is this in CSL? <text variable=\"type\" prefix=\" type={\" suffix=\"}\"/>--> \
--             \ <text variable=\"collection-title\" prefix=\" series={\" suffix=\"}\"/> \
--             \ <text macro=\"title\" prefix=\" title={\" suffix=\"}\"/> \
--             \ <text variable=\"volume\" prefix=\" volume={\" suffix=\"}\"/> \
--             \ <!--Not in CSL<text variable=\"rights\" prefix=\" rights={\" suffix=\"}\"/>--> \
--             \ <text variable=\"ISBN\" prefix=\" ISBN={\" suffix=\"}\"/> \
--             \ <text variable=\"ISSN\" prefix=\" ISSN={\" suffix=\"}\"/> \
--             \ <!--Not in CSL <text variable=\"LCCN\" prefix=\" callNumber={\" suffix=\"}\"/>--> \
--             \ <text variable=\"archive_location\" prefix=\" archiveLocation={\" suffix=\"}\"/> \
--             \ <text variable=\"URL\" prefix=\" url={\" suffix=\"}\"/> \
--             \ <text variable=\"DOI\" prefix=\" DOI={\" suffix=\"}\"/> \
--             \ <!-- <text variable=\"abstract\" prefix=\" abstractNote={\" suffix=\"}\"/> --> \
--             \ <text variable=\"note\" prefix=\" note={\" suffix=\"}\"/> \
--             \ <text macro=\"number\" prefix=\" number={\" suffix=\"}\"/> \
--             \ <text macro=\"container-title\"/> \
--             \ <text macro=\"publisher\"/> \
--             \ <text macro=\"author\" prefix=\" author={\" suffix=\"}\"/> \
--             \ <text macro=\"editor-translator\" prefix=\" editor={\" suffix=\"}\"/> \
--             \ <text macro=\"issued-year\" prefix=\" year={\" suffix=\"}\"/> \
--             \ <text macro=\"issued-month\" prefix=\" month={\" suffix=\"}\"/> \
--             \ <text macro=\"pages\" prefix=\" pages={\" suffix=\"}\"/> \
--             \ <text variable=\"collection-title\" prefix=\" collection={\" suffix=\"}\"/> \
--         \ </group> \
--         \ </layout> \
--     \ </bibliography> \
--     \ </style>"
