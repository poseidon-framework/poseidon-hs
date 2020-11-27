{-# LANGUAGE OverloadedStrings #-}
module Poseidon.BibFile (loadBibTeXFile, bibToSimpleMaybeList, writeBibTeXFile) where

import Poseidon.Utils (PoseidonException)

import           Control.Exception          (try)
import           Data.Either.Combinators    (rightToMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import           Paths_poseidon_hs          (getDataFileName)
import           Text.CSL                   (renderPlain, procOpts, processBibliography, readCSLFile)
import           Text.CSL.Exception         (CiteprocException)
import           Text.CSL.Input.Bibtex      (readBibtex)
import           Text.CSL.Reference         (Reference(..))

bibToSimpleMaybeList :: [Either PoseidonException (Either CiteprocException [Reference])] -> [Maybe [Reference]]
bibToSimpleMaybeList = map (maybe Nothing rightToMaybe . rightToMaybe)

-- BibTeX file parsing
writeBibTeXFile ::  FilePath -> [Reference] -> IO()
writeBibTeXFile path references = do
    bibTeXCSLPath <- getDataFileName "bibtex.csl"
    bibTeXCSLStyle <- readCSLFile Nothing bibTeXCSLPath
    let renderedReferences = processBibliography procOpts bibTeXCSLStyle references
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

loadBibTeXFiles :: [FilePath] -> IO [Either CiteprocException [Reference]]
loadBibTeXFiles bibPaths = do
    mapM loadBibTeXFile bibPaths

loadBibTeXFile :: FilePath -> IO (Either CiteprocException [Reference])
loadBibTeXFile bibPath = do
     try (readBibtex (const True) True False bibPath)