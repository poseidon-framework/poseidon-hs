{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.Core.ServerHTML (mainPage, archivePage, packageVersionPage, samplePage, PlotSample(..)) where

import           Poseidon.Core.ColumnTypesJanno
import           Poseidon.Core.ColumnTypesUtils (getListColumn)
import           Poseidon.Core.EntityTypes
import           Poseidon.Core.Janno
import           Poseidon.Core.Package
import Poseidon.Core.ServerStylesheet

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
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/pico.css"
    H.style ! A.type_ "text/css" $ H.preEscapedToHtml cssText
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

samplesJsonScript :: T.Text -> H.Html
samplesJsonScript samples =
  H.script
    ! A.id "samples-json"
    ! A.type_ "application/json"
    $ H.preEscapedToHtml (escapeJsonForScript samples)

escapeJsonForScript :: T.Text -> T.Text
escapeJsonForScript =
    T.replace "&" "\\u0026"
  . T.replace ">" "\\u003e"
  . T.replace "<" "\\u003c"

plots = do
    H.div ! A.id "timelineid" ! A.style "height: 120px; width: 100%;" $ ""
    H.div ! A.style "font-size: 12px; margin-bottom: 10px;" $ do
      H.toMarkup $ H.string "Only considers one median age per sample, binned in 100 year bins."
    H.div ! A.id "mapid" ! A.style "height: 350px;" $ ""

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
      samplesJsonScript (dataToJSON plotSamples)
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml jsText
    H.h1 (H.toMarkup $ "Archive: " <> archiveName)
    plots
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
      samplesJsonScript (dataToJSON plotSamples)
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml jsText
    case pacVersion of
      Nothing -> H.h1 (H.toMarkup $ "Package: " <> pacName)
      Just v -> H.h1 (H.toMarkup $ "Package: " <> pacName <> "-" <> showVersion v)
    plots
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
      samplesJsonScript (dataToJSON [plotSample])
      H.script ! A.type_ "text/javascript" $ H.preEscapedToHtml jsText
    H.h1 (H.toMarkup $ "Sample: " <> show (jPoseidonID row))
    plots
    H.div $ H.table $ do
      H.tr $ do
        H.th $ H.b "Property"
        H.th $ H.b "Value"
      forM_ (makeHeaderWithAdditionalColumns [row]) $ \key -> do
        H.tr $ do
          H.td $ H.toMarkup $ T.decodeUtf8Lenient key
          H.td $ H.toMarkup $ T.decodeUtf8Lenient $ HM.findWithDefault "" key hashMap


