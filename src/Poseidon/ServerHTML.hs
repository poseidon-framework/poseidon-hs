{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.ServerHTML (mainPage, archivePage, packageVersionPage, samplePage, PacVersion (..)) where

import           Poseidon.EntityTypes
import           Poseidon.Janno
import           Poseidon.Package

import qualified Data.Text                   as T
import           Data.Version                (Version, showVersion)
import           NeatInterpolation
import           Paths_poseidon_hs           (version)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Text
import qualified Web.Scotty                  as S

data PacVersion =
      Latest
    | NumericalVersion Version

instance Show PacVersion where
  show Latest               = "latest"
  show (NumericalVersion v) = showVersion v

renderMaybeVersion :: Maybe Version -> String
renderMaybeVersion Nothing  = ("" :: String)
renderMaybeVersion (Just v) = showVersion v

explorerPage :: H.Html -> H.Html
explorerPage content = do
    H.docType
    H.html $ do
      header
      H.body $ do
        H.main $ do
          navBar
          content
          footer

header :: H.Markup
header = H.head $ do
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/styles.css"
    -- leaflet (js must be after css)
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"
           ! H.customAttribute "integrity" "sha256-p4NxAoJBhIIN+hmNHrzRCf9tD/miZyoHS5obTRR9BMY="
           ! H.customAttribute "crossorigin" ""
    H.script ! A.src "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"
             ! H.customAttribute "integrity" "sha256-20nQCchB9co0qIjJZRGuk2/Z9VM+kNiyxNV1lvTlZBo="
             ! H.customAttribute "crossorigin" ""
             $ ""
    -- leaflet markercluster
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css"
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css"
    H.script ! A.src "https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js" $ ""
    -- custom javascript code
    H.text jscript
    H.script ! A.type_ "text/javascript" $ H.text jscript

jscript :: T.Text
jscript = [text|
window.onload = function() {
    console.log("Starting to load map");
    var mymap = L.map('mapid').setView([51.505, -0.09], 13);
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 19,
        attribution: 'Map data <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors'
    }).addTo(mymap);
    console.log("Map loaded successfully");
}
|]

navBar :: H.Html
navBar = H.nav $ do
  H.ul $ do
    H.li $ H.strong "Poseidon data explorer"
  H.ul $ do
    H.li $ H.a ! A.href "https://www.poseidon-adna.org" $ "Poseidon?"

footer :: H.Html
footer = H.footer ! A.style "border-top: 1px solid; padding: 1em; border-color: #727B8A;" $ do
    H.div ! A.style "float: left; font-size: 0.7em;" $ do
       "trident v" <> H.toMarkup (showVersion version)
    H.div ! A.style "float: right; font-size: 0.7em;" $ do
       "Built with "
       H.a ! A.href "https://picocss.com" $ "pico CSS"

mainPage :: [String] -> [[PoseidonPackage]] -> S.ActionM ()
mainPage archiveNames pacsPerArchive = S.html $ renderMarkup $ explorerPage $ do
  H.h1 "Archives"
  H.ul $ mapM_ (\(archiveName, pacs) -> do
    let nrPackages = length pacs
    H.article $ do
      H.header $ do
        H.a ! A.href ("/" <> H.toValue archiveName) $
          H.toMarkup archiveName
      H.toMarkup $ show nrPackages <> " packages"
      -- cover special cases for the main archive explorer website
      case archiveName of
        "community-archive" -> do
          H.br
          H.br
          H.p $ H.toMarkup (
            "Poseidon Community Archive (PCA) with per-paper packages and \
            \genotype data as published." :: String)
          H.footer $ H.p $ H.a
            ! A.href "https://github.com/poseidon-framework/community-archive"
            ! A.style "float: right; font-size: 0.8em;" $
            H.toMarkup ("The PCA on GitHub" :: String)
        "minotaur-archive" -> do
          H.br
          H.br
          H.p $ H.toMarkup (
            "Poseidon Minotaur Archive (PMA) with per-paper packages and \
            \genotype data reprocessed by the Minotaur workflow." :: String)
          H.footer $ H.p $ H.a
            ! A.href "https://github.com/poseidon-framework/minotaur-archive"
            ! A.style "float: right; font-size: 0.8em;" $
            H.toMarkup ("The PMA on GitHub" :: String)
        "aadr-archive" -> do
          H.br
          H.br
          H.p $ H.toMarkup (
            "Poseidon AADR Archive (PAA) with a structurally unified version of the \
            \AADR dataset repackaged in the Poseidon package format." :: String)
          H.footer $ H.p $ H.a
            ! A.href "https://github.com/poseidon-framework/aadr-archive"
            ! A.style "float: right; font-size: 0.8em;" $
            H.toMarkup ("The PAA on GitHub" :: String)
        _ -> return ()
    ) $ zip archiveNames pacsPerArchive

archivePage :: String -> [PoseidonPackage] -> S.ActionM ()
archivePage archiveName pacs = S.html $ renderMarkup $ explorerPage $ do
  H.h1 (H.toMarkup $ "Archive: " <> archiveName)
  H.div ! A.id "mapid" ! A.style "height: 300px;" $ ""
  H.ul $ mapM_ (\pac -> H.li $ H.div $ do
      let pacName = getPacName pac
          pacVersion = getPacVersion pac
      H.a ! A.href ("/" <>  H.toValue archiveName <> "/" <> H.toValue pacName) $
        H.toMarkup pacName
      H.toMarkup (" | " :: String)
      H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> H.toValue (renderMaybeVersion pacVersion)) $
        H.toMarkup ("Download" :: String)
    ) pacs

packageVersionPage :: String -> String -> PacVersion -> [PoseidonPackage] -> [JannoRow] -> S.ActionM ()
packageVersionPage archiveName pacName pacVersion pacs jannoRows = S.html $ renderMarkup $ explorerPage $ do
  H.h1 (H.toMarkup $ "Package: " <> pacName <> "-" <> show pacVersion)
  H.ul $ mapM_ (\pac -> H.li $ H.div $ do
       let v = getPacVersion pac
       H.a ! A.href ("/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (renderMaybeVersion v)) $
           H.toMarkup $ renderMaybeVersion v
    ) pacs
  H.table $ do
    H.tr $ do
      H.th $ H.b "PoseidonID"
      H.th $ H.b "Genetic_Sex"
      H.th $ H.b "Group_Name"
    mapM_ (\jannoRow -> do
        let link = "/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (show pacVersion) <> "/" <> H.toValue (jPoseidonID jannoRow)
        H.tr $ do
          H.td (H.a ! A.href link $ (H.toMarkup $ jPoseidonID jannoRow))
          H.td $ H.toMarkup $ show $ jGeneticSex jannoRow
          H.td $ H.toMarkup $ T.intercalate ", " $ map (\(GroupName t) -> t) $ getListColumn $ jGroupName jannoRow
      ) jannoRows

samplePage :: JannoRow -> S.ActionM ()
samplePage row = S.html $ renderMarkup $ explorerPage $ do
  H.h1 (H.toMarkup $ "Sample: " <> jPoseidonID row)
  H.table $ do
    H.tr $ do
      H.th $ H.b "Property"
      H.th $ H.b "Value"
    H.tr $ do
      H.td "PoseidonID"
      H.td (H.toMarkup $ jPoseidonID row)
    H.tr $ do
      H.td "GeneticSex"
      H.td (H.toMarkup $ show $ jGeneticSex row)
    H.tr $ do
      H.td "..."
      H.td (H.toMarkup ("..." :: String))


