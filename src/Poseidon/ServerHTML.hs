{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.ServerHTML (mainPage, archivePage, packageVersionPage, samplePage, MapMarker(..)) where

import           Poseidon.EntityTypes
import           Poseidon.Janno
import           Poseidon.Package

import           Control.Monad               (forM_)
import qualified Control.Monad               as OP
import           Data.Aeson                  (defaultOptions, encode,
                                              genericToEncoding)
import           Data.Aeson.Types            (ToJSON (..))
import qualified Data.ByteString.Lazy.Char8  as C
import           Data.Csv                    (ToNamedRecord (..))
import qualified Data.HashMap.Strict         as HM
import           Data.List                   (foldl', sortBy)
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Version                (Version, showVersion)
import           GHC.Generics
import           NeatInterpolation
import           Network.Wai                 (Request (..))
import           Paths_poseidon_hs           (version)
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Text
import qualified Web.Scotty                  as S

-- helper functions and types

renderMaybeVersion :: Maybe Version -> String
renderMaybeVersion Nothing  = ("" :: String)
renderMaybeVersion (Just v) = showVersion v

data MapMarker = MapMarker {
      mmLat            :: Double
    , mmLon            :: Double
    , mmPoseidonID     :: String
    , mmPackageName    :: String
    , mmPackageVersion :: Maybe String
    , mmArchiveName    :: String
    , mmLocation       :: Maybe String
    , mmAge            :: Maybe String
    } deriving (Generic, Show)

instance ToJSON MapMarker where
    toEncoding = genericToEncoding defaultOptions

dataToJSON :: ToJSON a => a -> T.Text
dataToJSON = T.pack . C.unpack . encode

-- javascript (leaflet map)

onloadJS :: T.Text -> T.Text -> T.Text
onloadJS nrLoaded mapMarkers = [text|
  window.onload = function() {
    
    // transform table to sortable version
    if (document.querySelector('#currentTable')) {
        let options = {
            searchable: true,
            perPage: 10
        };
        new simpleDatatables.DataTable('#currentTable', options);
    }
    
    // leaflet map
    if (document.querySelector('#mapid')) {
        var mymap = L.map('mapid').setView([35, 10], 1);
        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 19,
            attribution: 'Map data <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors'
        }).addTo(mymap);
        // add legend
        const nrLoaded = $nrLoaded;
        var legend = L.control({position: 'bottomright'});
        legend.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'info legend');
            div.innerHTML = nrLoaded[0] + ' samples loaded<br>' + nrLoaded[1] + ' lat/lon missing<br>';
            return div;
        };
        legend.addTo(mymap);
        // markers
        var markers = L.markerClusterGroup();
        const mapMarkers = $mapMarkers;
        for (var i = 0; i<mapMarkers.length; i++) {
            const s = mapMarkers[i];
            // prepare popup message
            const packageLink = '<a href="/explorer/' + s.mmArchiveName + '/' + s.mmPackageName + '/' + s.mmPackageVersion + '/' + s.mmPoseidonID + '" style="text-decoration: underline; cursor: pointer;">Open sample</a>';
            const popupContentLines = [];
            popupContentLines.push('<b>Poseidon ID:</b> ' + s.mmPoseidonID);
            popupContentLines.push('<b>Package:</b> ' + s.mmPackageName);
            popupContentLines.push('<b>Package version:</b> ' + s.mmPackageVersion);
            popupContentLines.push('<b>Archive:</b> ' + s.mmArchiveName);
            popupContentLines.push('<b>Location:</b> ' + s.mmLocation);
            popupContentLines.push('<b>Age BC/AD:</b> ' + s.mmAge);
            popupContentLines.push('<b>' + packageLink + '</b>');
            const popupContent = popupContentLines.join("<br>");
            // create a marker with a popup
            L.marker([s.mmLat, s.mmLon]).bindPopup(popupContent).addTo(markers);
        }
        mymap.addLayer(markers);
    }
  }
|]

-- css (specific additions to the stylesheet)

mapCSS :: T.Text
mapCSS = [text|
  /* overwrite some pico styling for the map */
  #mapid,
  #mapid * {
    padding: 0;
    --pico-border-width: 0rem !important;
    --pico-background-color: transparent !important;
  }
  /* legend */
  .legend {
    padding: 6px 8px !important;
    font: 14px/16px Arial, Helvetica, sans-serif;
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
    color: #777;
  }
  .leaflet-popup-content-wrapper {
    padding: 6px 8px !important;
  }
  /* overwrite some styling for the sortable table */
  .datatable-active button {
    color: #13171F !important;
  }
|]

-- html template

explorerPage :: [T.Text] -> H.Html -> H.Html
explorerPage urlPath content = do
    H.docType
    H.html $ do
      header
      H.body $ do
        H.main $ do
          navBar
          breadcrumb urlPath
          content
          footer

header :: H.Markup
header = H.head $ do
    -- load classless pico CSS
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/styles.css"
    H.style ! A.type_ "text/css" $ H.preEscapedToHtml mapCSS
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
    -- DataTables
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://cdn.jsdelivr.net/npm/simple-datatables@10.0/dist/style.css"
    H.script ! A.src "https://cdn.jsdelivr.net/npm/simple-datatables@10.0" $ ""

navBar :: H.Html
navBar = H.nav $ do
  H.ul $ do
    H.li $ H.strong "Poseidon data explorer"
  H.ul $ do
    H.li $ H.a ! A.href "https://www.poseidon-adna.org" $ "Poseidon?"

breadcrumb :: [T.Text] -> H.Html
breadcrumb segments =
  H.div ! A.style "font-size: 0.7em;" $
    mapM_ toLi $ zip segments paths
  where
    paths = tail (scanl (\acc seg -> acc <> "/" <> seg) "" segments)
    toLi (seg, path) = do
        H.a ! A.href (H.toValue $ T.unpack path) $ H.toMarkup seg
        H.text " / "

footer :: H.Html
footer = H.footer ! A.style "border-top: 1px solid; padding: 1em; border-color: #727B8A;" $ do
    H.div ! A.style "float: left; font-size: 0.7em;" $ do
       "trident v" <> H.toMarkup (showVersion version)
    H.div ! A.style "float: right; font-size: 0.7em;" $ do
       "Built with "
       H.a ! A.href "https://picocss.com" $ "pico CSS"

-- html pages

mainPage :: [(String, Maybe String, Maybe String,[PoseidonPackage])] -> S.ActionM ()
mainPage pacsPerArchive = do
  urlPath <- pathInfo <$> S.request
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.h1 "Archives"
    H.ul $ forM_ pacsPerArchive $ \(archiveName, maybeDescription, maybeURL, pacs) -> do
      let nrPackages = length pacs
      H.article $ do
        H.header $ do
          H.a ! A.href ("/explorer/" <> H.toValue archiveName) $
            H.toMarkup archiveName
        -- normal archive
        H.toMarkup $ show nrPackages <> " packages"
        H.br
        H.div ! A.style "font-size: 12px;" $ do
          H.toMarkup $ H.string "Last modified: "
          forM_ (take 3 $ sortBy (flip (\p1 p2 -> compare (posPacLastModified p1) (posPacLastModified p2))) pacs) $ \pac -> do
              let pacName = getPacName pac
                  pacNameVersion = renderNameWithVersion pac
              H.a ! A.href ("/explorer/" <>  H.toValue archiveName <> "/" <> H.toValue pacName) $ H.toMarkup pacNameVersion
              H.toMarkup $ H.string $ " (" ++  maybe "?" show (posPacLastModified pac) ++ "); "
        -- archives with more info
        case (maybeDescription,maybeURL) of
          (Just desc, Just url) -> do
            H.br
            H.br
            H.p $ H.toMarkup desc
            H.footer $ H.p $ H.a
              ! A.href (H.stringValue url)
              ! A.style "float: right; font-size: 0.8em;" $
              H.toMarkup ("Source archive" :: String)
          _ -> return ()

archivePage ::
     String
  -> Maybe String
  -> Bool
  -> [MapMarker]
  -> [PoseidonPackage]
  -> S.ActionM ()
archivePage archiveName maybeArchiveSpecURL archiveZip mapMarkers pacs = do
  urlPath <- pathInfo <$> S.request
  let nrSamplesTotal = foldl' (\i p -> i + length (getJannoRows $ posPacJanno p)) 0 pacs
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS (dataToJSON (length mapMarkers, nrSamplesTotal - length mapMarkers)) (dataToJSON mapMarkers))
    H.h1 (H.toMarkup $ "Archive: " <> archiveName)
    H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""
    H.div $ H.table ! A.id "currentTable" $ do
      H.thead $ do
          H.tr $ do
            H.th $ H.b "Package"
            H.th $ H.b "# Samples"
            H.th $ H.b "Last modified"
            H.th $ H.b "Source"
            H.th $ H.b ".zip Archive"
      forM_ pacs $ \pac -> do
        let pacName = getPacName pac
            nrSamples = length $ getJannoRows $ posPacJanno pac
            lastMod = posPacLastModified pac
        H.tr $ do
          -- normal archive
          H.td (H.a ! A.href ("/explorer/" <>  H.toValue archiveName <> "/" <> H.toValue pacName) $ H.toMarkup pacName)
          H.td $ H.toMarkup $ show nrSamples
          case lastMod of
            Just x -> H.td $ H.toMarkup $ show x
            Nothing -> H.td $ H.string "n/a"
          -- archives with more info
          case maybeArchiveSpecURL of
            Just url -> H.td $ H.a ! A.href (H.stringValue url <> "/" <> H.toValue pacName) $ H.toMarkup ("GitHub" :: String)
            Nothing  -> H.td $ H.string "n/a"
          if archiveZip
          then H.td $ H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?archive=" <> H.toValue archiveName) $ H.toMarkup ("Download" :: String)
          else H.td $ H.string "n/a"

packageVersionPage ::
     String -> String -> Maybe Version
  -> Bool
  -> [MapMarker]
  -> String
  -> PoseidonPackage -> [PoseidonPackage] -> [JannoRow]
  -> S.ActionM ()
packageVersionPage
  archiveName pacName pacVersion
  archiveZip
  mapMarkers
  bib
  oneVersion allVersions samples = do
  urlPath <- pathInfo <$> S.request
  let nrSamples = length $ getJannoRows $ posPacJanno oneVersion
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS (dataToJSON (length mapMarkers, nrSamples - length mapMarkers)) (dataToJSON mapMarkers))
    case pacVersion of
      Nothing -> H.h1 (H.toMarkup $ "Package: " <> pacName)
      Just v -> H.h1 (H.toMarkup $ "Package: " <> pacName <> "-" <> showVersion v)
    H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""
    H.br
    -- description
    H.article $ do
      H.b "Description: "
      H.toMarkup $ fromMaybe "unknown" (posPacDescription oneVersion)
      H.br
      H.b "Version: "
      H.toMarkup $ renderMaybeVersion $ getPacVersion oneVersion
      H.br
      H.b "Last modified: "
      H.toMarkup $ maybe "unknown" show (posPacLastModified oneVersion)
      H.br
      H.b "Number of samples: "
      H.toMarkup $ show nrSamples
    -- versions and bibliography
    H.div ! A.style "float: left; width: 70%;" $ do
      H.details $ do
        H.summary "Package versions"
        H.ul $ do
          forM_ allVersions $ \pac -> H.li $ H.div $ do
            let v = getPacVersion pac
            H.a ! A.href ("/explorer/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (renderMaybeVersion v)) $
              H.toMarkup $ renderMaybeVersion v
            OP.when archiveZip $ do
              H.toMarkup (" | " :: String)
              H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> H.toValue (renderMaybeVersion v) <> "&archive=" <> H.toValue archiveName) $
                H.toMarkup ("Download" :: String)
      H.details $ do
        H.summary "Bibliography (in bibtex format)"
        H.textarea ! A.rows "15" $ H.toMarkup bib
    -- download button
    OP.when archiveZip $
      H.div ! A.style "float: right; text-align: right;" $ do
        case pacVersion of
          Nothing -> do
            H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?archive=" <> H.toValue archiveName) $
              H.toMarkup ("Download" :: String)
          Just v -> do
            H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> H.toValue (showVersion v) <> "&archive=" <> H.toValue archiveName) $
              H.toMarkup ("Download" :: String)
    -- sample table
    H.div ! A.style "clear: both;" $ H.table ! A.id "currentTable" $ do
      H.thead $ do
        H.tr $ do
          H.th $ H.b "PoseidonID"
          H.th $ H.b "Genetic_Sex"
          H.th $ H.b "Group_Name"
      forM_ samples $ \jannoRow -> do
        let link = "/explorer/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (renderMaybeVersion pacVersion) <> "/" <> H.toValue (jPoseidonID jannoRow)
        H.tr $ do
          H.td $ H.a ! A.href link $ H.toMarkup $ jPoseidonID jannoRow
          H.td $ H.toMarkup $ show $ jGeneticSex jannoRow
          H.td $ H.toMarkup $ T.intercalate ", " $ map (\(GroupName t) -> t) $ getListColumn $ jGroupName jannoRow

samplePage ::
     Maybe MapMarker
  -> JannoRow
  -> S.ActionM ()
samplePage maybeMapMarker row = do
  urlPath <- pathInfo <$> S.request
  let hashMap = toNamedRecord row
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      case maybeMapMarker of
        Just mapMarker -> H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS (dataToJSON ((1,0) :: (Int,Int))) (dataToJSON [mapMarker]))
        Nothing -> pure ()
    H.h1 (H.toMarkup $ "Sample: " <> jPoseidonID row)
    case maybeMapMarker of
      Just _  -> H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""
      Nothing -> pure ()
    H.div $ H.table $ do
      H.tr $ do
        H.th $ H.b "Property"
        H.th $ H.b "Value"
      forM_ (makeHeaderWithAdditionalColumns [row]) $ \key -> do
        H.tr $ do
          H.td $ H.toMarkup $ T.decodeUtf8Lenient key
          H.td $ H.toMarkup $ T.decodeUtf8Lenient $ HM.findWithDefault "" key hashMap


