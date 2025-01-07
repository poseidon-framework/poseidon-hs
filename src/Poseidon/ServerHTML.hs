{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Poseidon.ServerHTML (mainPage, archivePage, packagePage, packageVersionPage, samplePage) where

import           Poseidon.EntityTypes
import           Poseidon.Janno
import           Poseidon.Package

import qualified Data.Text                   as T
import           Data.Version                (Version, showVersion)
import           NeatInterpolation
import qualified Text.Blaze.Html5            as H
import           Text.Blaze.Html5            ((!))
import qualified Text.Blaze.Html5.Attributes as A
import           Text.Blaze.Renderer.Text
import qualified Web.Scotty                  as S

renderMaybeVersion :: Maybe Version -> String
renderMaybeVersion Nothing  = ("" :: String)
renderMaybeVersion (Just v) = showVersion v

jscript :: T.Text
jscript = [text|


|]

headerWithCSS :: H.Markup
headerWithCSS = H.head $ do
    H.script ! A.type_ "text/javascript" $ H.text jscript
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.classless.blue.min.css"

mainPage :: [String] -> [[PoseidonPackage]] -> S.ActionM ()
mainPage archiveNames pacsPerArchive = S.html $ renderMarkup $ do
  H.html $ do
    headerWithCSS
    H.body $ do
      H.main $ do
        H.h1 "Archives"
        H.ul $ mapM_ (\(archiveName, pacs) -> do
          let nrPackages = length pacs
          H.article $ do
            H.header $ do
              H.a ! A.href ("/" <> H.toValue archiveName) $
                H.toMarkup archiveName
            H.toMarkup $ (show nrPackages) <> " packages"
          ) $ zip archiveNames pacsPerArchive

archivePage :: String -> [PoseidonPackage] -> S.ActionM ()
archivePage archiveName pacs = S.html $ renderMarkup $ do
  H.html $ do
    headerWithCSS
    H.body $ do
      H.main $ do
        H.h1 (H.toMarkup $ "Archive: " <> archiveName)
        H.ul $ mapM_ (\pac -> H.li $ H.div $ do
            let pacName = getPacName pac
                pacVersion = getPacVersion pac
            H.a ! A.href ("/" <>  H.toValue archiveName <> "/" <> H.toValue pacName) $
              H.toMarkup pacName
            H.toMarkup (" | " :: String)
            H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> H.toValue (renderMaybeVersion pacVersion)) $
              H.toMarkup ("Download" :: String)
          ) pacs

packagePage :: String -> String -> [PoseidonPackage] -> S.ActionM ()
packagePage archiveName pacName pacs = S.html $ renderMarkup $ do
  H.html $ do
    headerWithCSS
    H.body $ do
      H.main $ do
        H.h1 (H.toMarkup $ "Package: " <> pacName)
        H.ul $ mapM_ (\pac -> H.li $ H.div $ do
             let version = getPacVersion pac
             H.a ! A.href ("/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (renderMaybeVersion version)) $
                 H.toMarkup $ renderMaybeVersion version
          ) pacs

packageVersionPage :: String -> String -> Version -> [JannoRow] -> S.ActionM ()
packageVersionPage archiveName pacName pacVersion jannoRows = S.html $ renderMarkup $ do
  H.html $ do
    headerWithCSS
    H.body $ do
      H.main $ do
        H.h1 (H.toMarkup $ "Package: " <> pacName <> "-" <> showVersion pacVersion)
        H.ul $ mapM_ (\jannoRow -> H.li $ H.div $ do
             H.a ! A.href ("/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (showVersion pacVersion) <> "/" <> H.toValue (jPoseidonID jannoRow)) $
                 H.toMarkup $ jPoseidonID jannoRow
          ) jannoRows

samplePage :: JannoRow -> S.ActionM ()
samplePage row = S.html $ renderMarkup $ do
  H.html $ do
    headerWithCSS
    H.body $ do
      H.main $ do
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


