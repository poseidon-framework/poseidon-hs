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
    , mmAgeStart       :: Maybe Int
    , mmAge            :: Maybe Int
    , mmAgeStop        :: Maybe Int
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

    // load samples and try to complete some mean age information
    const samples = $samples.map(s => {
      let mmAge = s.mmAge;
      if (mmAge == null &&
          s.mmAgeStart != null &&
          s.mmAgeStop != null &&
          Number.isFinite(s.mmAgeStart) &&
          Number.isFinite(s.mmAgeStop)) {
        mmAge = Math.round((s.mmAgeStart + s.mmAgeStop) / 2);
      }
      return {...s, mmAge: mmAge};
    });

    // prepare samples for plotting
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

    function formatYear(x) {
      if (x == null || Number.isNaN(Number(x))) { return 'unknown'; }
      x = Number(x);
      return x < 0 ? `${Math.abs(x).toLocaleString()} BC` : `${x.toLocaleString()} AD`;
    }

    // timeline plot with Vega-Lite
    if (document.querySelector('#timelineid')) {
        const timelineEl = document.querySelector('#timelineid');
        if (timelineMarkers.length === 0) {
            timelineEl.innerHTML = '<p>No samples with usable age information.</p>';
        } else {
            const binWidth = 1000;
            // bin data
            const bins = new Map();
            for (const s of timelineMarkers) {
                const age = Number(s.mmAge);
                if (!Number.isFinite(age)) continue;
                const binStart = Math.floor(age / binWidth) * binWidth;
                const binEnd = binStart + binWidth;
                const key = `${binStart}:${binEnd}`;
                if (!bins.has(key)) {
                    bins.set(key, {
                        binStart,
                        binEnd,
                        count: 0,
                        ids: []
                    });
                }
                const entry = bins.get(key);
                entry.count += 1;
                entry.ids.push(s.mmPoseidonID);
            }
            const timelineBins = Array.from(bins.values())
                .sort((a, b) => a.binStart - b.binStart)
                .map(d => {
                    const idsPreview = d.ids.length > 20
                        ? d.ids.slice(0, 20).join(', ') + `, … ${d.ids.length - 20} more`
                        : d.ids.join(', ');
                    return {
                        binStart: d.binStart,
                        binEnd: d.binEnd,
                        count: d.count,
                        binLabel: `${formatYear(d.binStart)} – ${formatYear(d.binEnd)}`,
                        idsPreview
                    };
                });
            const ages = timelineMarkers
                .map(s => Number(s.mmAge))
                .filter(Number.isFinite);
            const dataMin = Math.min(...ages);
            const dataMax = Math.max(...ages);
            const domainStart = Math.floor(dataMin / binWidth) * binWidth;
            const domainEnd = Math.ceil(dataMax / binWidth) * binWidth;
            const timelineSpec = {
                $schema: 'https://vega.github.io/schema/vega-lite/v6.json',
                width: 'container',
                height: 260,
                autosize: { type: 'fit', contains: 'padding' },
                data: { values: timelineBins },
                params: [
                    {
                        name: 'x_zoom',
                        select: {
                            type: 'interval',
                            encodings: ['x']
                        },
                        bind: 'scales'
                    }
                ],
                mark: { type: 'bar', color: '#4C78A8' },
                encoding: {
                    x: {
                        field: 'binStart',
                        type: 'quantitative',
                        title: 'Age',
                        scale: {
                            domain: [domainStart, domainEnd],
                            nice: false
                        },
                        axis: {
                            grid: true,
                            tickCount: 12,
                            labelExpr: `
                                datum.value < 0
                                  ? format(abs(datum.value), ',') + ' BC'
                                  : format(datum.value, ',') + ' AD'
                            `
                        }
                    },
                    x2: { field: 'binEnd' },
                    y: {
                        field: 'count',
                        type: 'quantitative',
                        title: 'Samples',
                        axis: { tickMinStep: 1 }
                    },
                    tooltip: [
                        {
                            field: 'binLabel',
                            type: 'nominal',
                            title: 'Interval'
                        },
                        {
                            field: 'count',
                            type: 'quantitative',
                            title: 'Samples'
                        },
                        {
                            field: 'idsPreview',
                            type: 'nominal',
                            title: 'Poseidon IDs'
                        }
                    ]
                },
                config: {
                    view: {
                        stroke: null
                    },
                    axis: {
                        labelFontSize: 11,
                        titleFontSize: 12
                    }
                }
            };
            vegaEmbed('#timelineid', timelineSpec, {
                actions: false,
                renderer: 'canvas'
            });
        }
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
        var markers = L.markerClusterGroup({
            chunkedLoading: true,
        });
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
            popupContentLines.push('<b>Age:</b> ' + formatYear(s.mmAge) + " (" + formatYear(s.mmAgeStart) + " - " + formatYear(s.mmAgeStop) + ")");
            popupContentLines.push('<b>' + packageLink + '</b>');
            const popupContent = popupContentLines.join("<br>");
            // create a marker with a popup
            L.marker([s.mmLat, s.mmLon])
            .bindTooltip(s.mmPoseidonID, {
              direction: 'right',
              sticky: true,
              opacity: 1,
              className: 'poseidon-tooltip'
            })
            .bindPopup(popupContent)
            .addTo(markers);
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
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "/styles.css"
    H.style ! A.type_ "text/css"
            $ H.preEscapedToHtml mapCSS
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
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.css"
           ! H.customAttribute "integrity" "sha256-YU3qCpj/P06tdPBJGPax0bm6Q1wltfwjsho5TR4+TYc="
           ! H.customAttribute "crossorigin" ""
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "https://unpkg.com/leaflet.markercluster@1.5.3/dist/MarkerCluster.Default.css"
           ! H.customAttribute "integrity" "sha256-YSWCMtmNZNwqex4CEw1nQhvFub2lmU7vcCKP+XVwwXA="
           ! H.customAttribute "crossorigin" ""
    H.script ! A.src "https://unpkg.com/leaflet.markercluster@1.5.3/dist/leaflet.markercluster.js"
             ! H.customAttribute "integrity" "sha256-Hk4dIpcqOSb0hZjgyvFOP+cEmDXUKKNE/tT542ZbNQg="
             ! H.customAttribute "crossorigin" ""
             $ ""
    -- DataTables
    H.link ! A.rel "stylesheet"
           ! A.type_ "text/css"
           ! A.href "https://cdn.jsdelivr.net/npm/simple-datatables@10.0/dist/style.css"
           ! H.customAttribute "integrity" "sha256-8URSbDdnZAN5R2+1U5VKQ+xD8I1krfrjf1ChUjbguEk="
           ! H.customAttribute "crossorigin" ""
    H.script ! A.src "https://cdn.jsdelivr.net/npm/simple-datatables@10.0"
             ! H.customAttribute "integrity" "sha256-x6Cva2xkJFDOJblsizzuGX3Y+ucjav8yEx3sRFmgDYk="
             ! H.customAttribute "crossorigin" ""
             $ ""
    -- vega-lite
    H.script ! A.src "https://cdn.jsdelivr.net/npm/vega@6.2.0"
             ! H.customAttribute "integrity" "sha256-6mpZNjgdBvkSRmNU4raiDrZsTUHxyp3/HgI23+5u45I="
             ! H.customAttribute "crossorigin" ""
             $ ""
    H.script ! A.src "https://cdn.jsdelivr.net/npm/vega-lite@6.4.3"
             ! H.customAttribute "integrity" "sha256-NamCHfg4glsFpqc+lBS1h0ehsYMhWDhY7ZA8Zjk6XH4="
             ! H.customAttribute "crossorigin" ""
             $ ""
    H.script ! A.src "https://cdn.jsdelivr.net/npm/vega-embed@7.1.0"
             ! H.customAttribute "integrity" "sha256-w2JUJwIZ7uWPubHZVN7K2VT7B7/Jq3gMXUQBvURc1Qw="
             ! H.customAttribute "crossorigin" ""
             $ ""

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
  -> [String]
  -> [PlotSample]
  -> [PoseidonPackage]
  -> S.ActionM ()
archivePage archiveName maybeArchiveSpecURL excludeFromMap plotSamples pacs = do
  urlPath <- pathInfo <$> S.request
  S.html $ renderMarkup $ explorerPage urlPath $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml (onloadJS $ dataToJSON plotSamples)
    H.h1 (H.toMarkup $ "Archive: " <> archiveName)
    H.div ! A.id "timelineid" ! A.style "height: 120px;" $ ""
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
          H.td $ H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?archive=" <> H.toValue archiveName) $ H.toMarkup ("Download" :: String)

packageVersionPage ::
     String -> String -> Maybe Version
  -> [PlotSample]
  -> String
  -> PoseidonPackage -> [PoseidonPackage] -> [JannoRow]
  -> S.ActionM ()
packageVersionPage
  archiveName pacName pacVersion
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
    H.div ! A.id "timelineid" ! A.style "height: 120px;" $ ""
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
            H.toMarkup (" | " :: String)
            H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> H.toValue (renderMaybeVersion v) <> "&archive=" <> H.toValue archiveName) $
                 H.toMarkup ("Download" :: String)
      H.details $ do
        H.summary "Bibliography (in bibtex format)"
        H.textarea ! A.rows "15" $ H.toMarkup bib
    -- download button
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
    H.div ! A.id "timelineid" ! A.style "height: 120px;" $ ""
    H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""
    H.div $ H.table $ do
      H.tr $ do
        H.th $ H.b "Property"
        H.th $ H.b "Value"
      forM_ (makeHeaderWithAdditionalColumns [row]) $ \key -> do
        H.tr $ do
          H.td $ H.toMarkup $ T.decodeUtf8Lenient key
          H.td $ H.toMarkup $ T.decodeUtf8Lenient $ HM.findWithDefault "" key hashMap


