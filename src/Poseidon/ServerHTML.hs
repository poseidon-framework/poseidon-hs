{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Poseidon.ServerHTML (mainPage, archivePage, packagePage, packageVersionPage, samplePage) where

import Poseidon.Package
import Poseidon.EntityTypes
import Poseidon.Janno

import qualified Web.Scotty as S
import Text.Blaze.Renderer.Text
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Data.Version (showVersion, Version)
import NeatInterpolation
import qualified Data.Text as T

renderMaybeVersion :: Maybe Version -> String
renderMaybeVersion Nothing  = ("" :: String)
renderMaybeVersion (Just v) = showVersion v

jscript :: T.Text
jscript = [text|


|]

mainPage :: [String] -> S.ActionM ()
mainPage archiveNames = S.html $ renderMarkup $ do
  H.html $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.text jscript
    H.body $ do
      H.h1 "Poseidon public archives"
      H.ul $ mapM_ (\archiveName -> H.li $ H.div $ do
              H.a ! A.href ("/" <> H.toValue archiveName) $
                H.toMarkup archiveName
          ) archiveNames

archivePage :: String -> [PoseidonPackage] -> S.ActionM ()
archivePage archiveName pacs = S.html $ renderMarkup $ do
  H.html $ do
    H.head $ do
      H.script ! A.type_ "text/javascript" $ H.text jscript
    H.body $ do
      H.h1 $ H.toMarkup archiveName
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
    H.body $ do
      H.h1 (H.toMarkup pacName)
      H.ul $ mapM_ (\pac -> H.li $ H.div $ do
           let version = getPacVersion pac
           H.a ! A.href ("/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (renderMaybeVersion version)) $
               H.toMarkup $ renderMaybeVersion version
        ) pacs
        
packageVersionPage :: String -> String -> Version -> [JannoRow] -> S.ActionM ()
packageVersionPage archiveName pacName pacVersion jannoRows = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 (H.toMarkup pacName)
      H.ul $ mapM_ (\jannoRow -> H.li $ H.div $ do
           H.a ! A.href ("/" <> H.toValue archiveName <> "/" <> H.toValue pacName <> "/" <> H.toValue (showVersion pacVersion) <> "/" <> H.toValue (jPoseidonID jannoRow)) $
               H.toMarkup $ jPoseidonID jannoRow
        ) jannoRows

samplePage :: JannoRow -> S.ActionM ()
samplePage row = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 (H.toMarkup $ jPoseidonID row)
      H.table $ do
        H.tr $ do
          H.th "Property"
          H.th "Value"
        H.tr $ do
          H.td "PoseidonID"
          H.td (H.toMarkup $ jPoseidonID row)
        H.tr $ do
          H.td "GeneticSex"
          H.td (H.toMarkup $ show $ jGeneticSex row)
        H.tr $ do
          H.td "..."
          H.td (H.toMarkup ("..." :: String))


