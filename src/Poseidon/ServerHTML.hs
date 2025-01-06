{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Poseidon.ServerHTML (mainPage, archivePage, packagePage) where

import Poseidon.Package
import Poseidon.EntityTypes

import qualified Web.Scotty as S
import Text.Blaze.Renderer.Text
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5.Attributes as A
import Data.Version (showVersion, Version)
import NeatInterpolation
import qualified Data.Text as T

renderMaybeVersion :: Maybe Version -> H.AttributeValue
renderMaybeVersion Nothing  = H.toValue ("" :: String)
renderMaybeVersion (Just v) = H.toValue $ showVersion v

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
              pacNameAndVersion = renderNameWithVersion $ posPacNameAndVersion pac
          H.a ! A.href ("/" <>  H.toValue archiveName <> "/" <> H.toValue pacName <> "?package_version=" <> renderMaybeVersion pacVersion) $
            H.toMarkup pacNameAndVersion
          H.toMarkup (" | " :: String)
          H.a ! A.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> renderMaybeVersion pacVersion) $
            H.toMarkup ("Download" :: String)
        ) pacs

packagePage :: PoseidonPackage -> S.ActionM ()
packagePage pac = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 (H.toMarkup $ renderNameWithVersion $ posPacNameAndVersion pac)