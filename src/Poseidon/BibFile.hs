{-# LANGUAGE OverloadedStrings #-}
module Poseidon.BibFile (loadBibTeXFile, bibToSimpleMaybeList, writeBibTeXFile) where

import           Poseidon.Utils          (PoseidonException(..))

import           Control.Exception       (throwIO, catch)
import           Control.Monad           (when)
import           Data.Either.Combinators (rightToMaybe)
import qualified Data.Text               as T
import qualified Data.Text.IO            as Tio
import           Paths_poseidon_hs       (getDataFileName)
import           System.Directory        (doesFileExist)
import           Text.CSL                (procOpts, processBibliography,
                                          readCSLFile, renderPlain)
import           Text.CSL.Exception      (CiteprocException)
import           Text.CSL.Input.Bibtex   (readBibtex)
import           Text.CSL.Reference      (Reference (..))

bibToSimpleMaybeList :: [Either PoseidonException [Reference]] -> [Maybe [Reference]]
bibToSimpleMaybeList = map rightToMaybe

-- BibTeX file parsing
writeBibTeXFile ::  FilePath -> [Reference] -> IO()
writeBibTeXFile path references_ = do
    bibTeXCSLPath <- getDataFileName "bibtex.csl"
    bibTeXCSLStyle <- readCSLFile Nothing bibTeXCSLPath
    let renderedReferences = processBibliography procOpts bibTeXCSLStyle references_
    let referencesTexts = map renderPlain renderedReferences
    let referencesTextsFixed = map cleanBibTeXString referencesTexts
    let huup = T.intercalate "\n\n" referencesTextsFixed
    Tio.writeFile path huup

cleanBibTeXString :: T.Text -> T.Text
cleanBibTeXString =
    T.replace "} }" "}\n}"
    . T.replace ", title=" ",\n  title="
    . T.replace "   " "  "
    . T.replace "}," "},\n  "
    . T.replace "\n" " "

loadBibTeXFile :: FilePath -> IO [Reference]
loadBibTeXFile bibPath = do
    fileE <- doesFileExist bibPath
    when (not fileE) . throwIO $ PoseidonFileExistenceException ("Could not find bib file " ++ bibPath)
    catch (readBibtex (const True) True False bibPath) handler
  where
    handler :: CiteprocException -> IO [Reference]
    handler e = throwIO $ PoseidonBibTeXException bibPath (show e)
