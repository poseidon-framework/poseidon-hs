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

jscript :: T.Text
jscript = [text|


|]

explorerPage :: H.Html -> H.Html
explorerPage content = do
    H.html $ do
      headerWithCSS
      H.body $ do
        H.main $ do
          navBar
          content
          footer

headerWithCSS :: H.Markup
headerWithCSS = H.head $ do
    H.script ! A.type_ "text/javascript" $ H.text jscript
    H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "/styles.css"

navBar :: H.Html
navBar = H.nav $ do
  H.ul $ do
    H.li $ H.strong $ "Poseidon data explorer"
  H.ul $ do
    H.li $ H.a ! A.href "https://www.poseidon-adna.org" $ "Poseidon?"

footer :: H.Html
footer = H.footer ! A.style "border-top: 1px solid; padding: 1em; border-color: #727B8A;" $ do
    H.div ! A.style "float: left; font-size: 0.7em;" $ do
       "trident version " <> H.toMarkup (showVersion version)
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
      H.toMarkup $ (show nrPackages) <> " packages"
    ) $ zip archiveNames pacsPerArchive

archivePage :: String -> [PoseidonPackage] -> S.ActionM ()
archivePage archiveName pacs = S.html $ renderMarkup $ explorerPage $ do
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


