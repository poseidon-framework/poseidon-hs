{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.Core.ServerHTML (mainPage, archivePage, packageVersionPage, samplePage, PlotSample(..)) where

import           Poseidon.Core.ColumnTypesJanno
import           Poseidon.Core.ColumnTypesUtils (getListColumn)
import           Poseidon.Core.EntityTypes
import           Poseidon.Core.Janno
import           Poseidon.Core.Package

import           Control.Monad                  (forM_)
import qualified Control.Monad                  as OP
import           Data.Aeson                     (defaultOptions, encode,
                                                 genericToEncoding)
import           Data.Aeson.Types               (ToJSON (..))
import qualified Data.ByteString.Char8          as BS
import qualified Data.ByteString.Lazy.Char8     as C
import           Data.Csv                       (ToNamedRecord (..))
import qualified Data.HashMap.Strict            as HM
import           Data.List                      (intercalate, sortBy)
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           Data.Version                   (Version, showVersion)
import           GHC.Generics
import           NeatInterpolation
import           Network.Wai                    (Request (..))
import           Paths_poseidon_hs              (version)
import qualified Text.Blaze.Html5               as H
import           Text.Blaze.Html5               ((!))
import qualified Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Renderer.Text
import qualified Web.Scotty                     as S

-- helper functions and types

renderMaybeVersion :: Maybe Version -> String
renderMaybeVersion Nothing  = ("" :: String)
renderMaybeVersion (Just v) = showVersion v

data PlotSample = PlotSample {
      mmLat            :: Maybe Double
    , mmLon            :: Maybe Double
    , mmPoseidonID     :: String
    , mmPackageName    :: String
    , mmPackageVersion :: Maybe String
    , mmArchiveName    :: String
    , mmLocation       :: Maybe String
    , mmAge            :: Maybe Int
    } deriving (Generic, Show)

instance ToJSON PlotSample where
    toEncoding = genericToEncoding defaultOptions

dataToJSON :: ToJSON a => a -> T.Text
dataToJSON = T.pack . C.unpack . encode

-- javascript (leaflet map)

onloadJS :: T.Text -> T.Text
onloadJS samples = [text|
  window.onload = function() {

    // transform table to sortable version
    if (document.querySelector('#currentTable')) {
        let options = {
            searchable: true,
            perPage: 10
        };
        new simpleDatatables.DataTable('#currentTable', options);
    }

    // prepare data for plotting
    const samples = $samples;
    const initialLength = samples.length;
    const mapMarkers = samples.filter(s =>
      s.mmLat != null &&
      s.mmLon != null &&
      Number.isFinite(s.mmLat) &&
      Number.isFinite(s.mmLon)
    );
    const timelineMarkers = samples.filter(s =>
      s.mmAge != null &&
      Number.isFinite(s.mmAge)
    );
    const missingCoordinates = initialLength - mapMarkers.length;
    const missingAge = initialLength - timelineMarkers.length;

    // helper functions
    function formatYear(x) {
      if (x == null || Number.isNaN(Number(x))) { return 'unknown'; }
      x = Number(x);
      return x < 0 ? `${Math.abs(x).toLocaleString()} BC` : `${x.toLocaleString()} AD`;
    }
    function getMeanAge(markers) {
      const valid = markers
        .map(s => Number(s.mmAge))
        .filter(Number.isFinite);
      if (valid.length === 0) return (xMin + xMax) / 2;
      return valid.reduce((sum, x) => sum + x, 0) / valid.length;
    }
    // inspired by ggpointgrid::geom_pointrect
    function makeBoxOffsets(count, maxRows) {
        var fact = Math.sqrt(count);
        var ncols = Math.ceil(fact);
        var nrows;
        if (ncols * Math.floor(fact) >= count) {
            nrows = Math.floor(fact);
        } else {
            nrows = Math.ceil(fact);
        }
        // cap rows if needed
        if (nrows > maxRows) {
            nrows = maxRows;
            ncols = Math.ceil(count / nrows);
        }
        var gridCenterX = (ncols + 1) / 2;
        var gridCenterY = (nrows + 1) / 2;
        var offsets = [];
        var col, row;
        for (row = nrows; row >= 1; row--) {
            for (col = 1; col <= ncols; col++) {
                offsets.push({
                    x: col - gridCenterX,
                    y: row - gridCenterY
                });
            }
        }
        return offsets.slice(0, count);
    }

    // timeline plot (implemented with leaflet)
    if (document.querySelector('#timelineid')) {
        // pseudo-map as plotting canvas
        const timeline = L.map('timelineid', {
          crs: L.CRS.Simple,
          minZoom: -5,
          maxZoom: 0, // must also be changed in disableClusteringAtZoom
          inertia: false,
          zoomControl: false,
          attributionControl: false
        });
        // plot bounds
        const xMin = -150000;
        const xMax = 2100;
        const yMin = 0;
        const yMax = 100;
        const bounds = [[yMin, xMin], [yMax, xMax]];
        // panning constraints
        timeline.setMaxBounds([[yMin, xMin - 2000], [yMax, xMax + 2000]]);
        // set view to data
        const meanX = getMeanAge(timelineMarkers);
        const centerY = (yMin + yMax) / 2;
        // first establish the zoom level from the full plot bounds
        timeline.fitBounds(bounds);
        timeline.setView([centerY, meanX], timeline.getZoom(), {
          animate: false
        });
        // layer group for axis
        const bottomAxisLayer = L.layerGroup().addTo(timeline);
        const AXIS_Y = 0;
        const SMALL_TICK_TOP = 40;
        const BIG_TICK_TOP = 50;
        const LABEL_Y = 50;
        function drawBottomAxis() {
          bottomAxisLayer.clearLayers();
          const bounds = timeline.getBounds();
          const westernBorder = bounds.getWest()
          const easternBorder = bounds.getEast()
          // bottom line
          L.polyline([[AXIS_Y, westernBorder], [AXIS_Y, easternBorder]], {
            color: 'black',
            weight: 1
          }).addTo(bottomAxisLayer);
          const minorStep = 100;
          const majorStep = 1000;
          const visibleXMin = Math.max(xMin, westernBorder);
          const visibleXMax = Math.min(xMax, easternBorder);
          const startTick = Math.floor(visibleXMin / minorStep) * minorStep;
          for (let x = startTick; x <= visibleXMax; x += minorStep) {
            const isMajor = (x % majorStep === 0);
            const tickTop = isMajor ? BIG_TICK_TOP : SMALL_TICK_TOP;
            const tickWeight = isMajor ? 2 : 1;
            L.polyline([[AXIS_Y, x], [tickTop, x]], {
              color: 'black',
              weight: tickWeight
            }).addTo(bottomAxisLayer);
            if (isMajor) {
              L.marker([LABEL_Y, x], {
                interactive: false,
                icon: L.divIcon({
                  className: 'bottom-axis-label',
                  html: `
                    <span style="
                      display:inline-block;
                      font-size: 10px;
                      color: black;
                      transform: rotate(25deg);
                      transform-origin: left top;
                      white-space: nowrap;">
                      ${formatYear(x)}
                    </span>
                  `,
                  iconSize: [60, 20],
                  iconAnchor: [0, 0]
                })
              }).addTo(bottomAxisLayer);
            }
          }
        }
        timeline.on('moveend zoomend resize', drawBottomAxis);
        drawBottomAxis();
        // add markers
        var markers = L.markerClusterGroup({
        	spiderfyShapePositions: function(count, centerPt) {
                var scaleX = 12;
                var scaleY = 12;
                var offsets = makeBoxOffsets(count, 5);
                return offsets.map(function(offset) {
                    return new L.Point(
                        centerPt.x + offset.x * scaleX,
                        centerPt.y + 20 + offset.y * scaleY
                    );
                });
            }
        });
        for (var i = 0; i<timelineMarkers.length; i++) {
          const s = timelineMarkers[i];
          L.marker([50, s.mmAge])
          .bindTooltip(s.mmPoseidonID, {
            direction: 'right',
            sticky: true,
            opacity: 1,
            className: 'poseidon-tooltip'
          })
          .addTo(markers);
        }
        timeline.addLayer(markers);
    }

    // leaflet map
    if (document.querySelector('#mapid')) {
        var mymap = L.map('mapid').setView([35, 10], 1);
        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            maxZoom: 19,
            attribution: 'Map data <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors'
        }).addTo(mymap);
        // add legend
        var legend = L.control({position: 'bottomright'});
        legend.onAdd = function (map) {
            var div = L.DomUtil.create('div', 'info legend');
            div.innerHTML = initialLength + ' samples loaded<br>' +
                            missingCoordinates + ' lat/lon missing<br>' +
                            missingAge + ' ages missing<br>';
            return div;
        };
        legend.addTo(mymap);
        // markers
        var markers = L.markerClusterGroup();
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
            popupContentLines.push('<b>Age:</b> ' + formatYear(s.mmAge));
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
  /* overwrite some pico styling for the charts */
  #timelineid.leaflet-container {
    background-color: white !important;
    border: 1px solid black !important;
    box-sizing: border-box;
  }
  #timelineid,
  #timelineid * {
    margin-bottom: 10px;
    padding: 0;
    --pico-border-width: 0rem !important;
    --pico-background-color: transparent !important;
  }
  .leaflet-container {
    background: white;
  }
  #mapid.leaflet-container {
    background-color: white !important;
    border: 1px solid black !important;
    box-sizing: border-box;
  }
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
  -> [String]
  -> [PlotSample]
  -> [PoseidonPackage]
  -> S.ActionM ()
archivePage archiveName maybeArchiveSpecURL archiveZip excludeFromMap plotSamples pacs = do
  urlPath <- pathInfo <$> S.request
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS $ dataToJSON plotSamples)
    H.h1 (H.toMarkup $ "Archive: " <> archiveName)
    H.div ! A.id "timelineid" ! A.style "height: 100px;" $ ""
    H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""
    case excludeFromMap of
      [] -> do
        H.div ! A.style "font-size: 12px;" $ do
          H.toMarkup $ H.string "No packages excluded from map."
      exclude -> do
        H.div ! A.style "font-size: 12px;" $ do
          H.toMarkup $ H.string $ "Packages excluded from map: " ++ intercalate ", " exclude
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
            Just x  -> H.td $ H.toMarkup $ show x
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
  -> [PlotSample]
  -> String
  -> PoseidonPackage -> [PoseidonPackage] -> [JannoRow]
  -> S.ActionM ()
packageVersionPage
  archiveName pacName pacVersion
  archiveZip
  plotSamples
  bib
  oneVersion allVersions samples = do
  urlPath <- pathInfo <$> S.request
  let nrSamples = length $ getJannoRows $ posPacJanno oneVersion
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS $ dataToJSON plotSamples)
    case pacVersion of
      Nothing -> H.h1 (H.toMarkup $ "Package: " <> pacName)
      Just v -> H.h1 (H.toMarkup $ "Package: " <> pacName <> "-" <> showVersion v)
    H.div ! A.id "timelineid" ! A.style "height: 150px;" $ ""
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
        let link = "/explorer/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (renderMaybeVersion pacVersion) <> "/" <> H.toValue (BS.unpack . unPoseidonID . jPoseidonID $ jannoRow)
        H.tr $ do
          H.td $ H.a ! A.href link $ H.toMarkup . T.pack . BS.unpack . unPoseidonID . jPoseidonID $ jannoRow
          H.td $ H.toMarkup $ show $ jGeneticSex jannoRow
          H.td . H.toMarkup . T.intercalate ", " . map (T.pack . BS.unpack . unGroupName) . getListColumn . jGroupName $ jannoRow

samplePage ::
     PlotSample
  -> JannoRow
  -> S.ActionM ()
samplePage plotSample row = do
  urlPath <- pathInfo <$> S.request
  let hashMap = toNamedRecord row
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS $ dataToJSON [plotSample])
    H.h1 (H.toMarkup $ "Sample: " <> show (jPoseidonID row))
    H.div ! A.id "timelineid" ! A.style "height: 150px;" $ ""
    H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""
    H.div $ H.table $ do
      H.tr $ do
        H.th $ H.b "Property"
        H.th $ H.b "Value"
      forM_ (makeHeaderWithAdditionalColumns [row]) $ \key -> do
        H.tr $ do
          H.td $ H.toMarkup $ T.decodeUtf8Lenient key
          H.td $ H.toMarkup $ T.decodeUtf8Lenient $ HM.findWithDefault "" key hashMap


