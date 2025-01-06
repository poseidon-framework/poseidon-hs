{-# LANGUAGE OverloadedStrings #-}

module Poseidon.ServerHTML (mainPage, packagePage) where

import Poseidon.Package
import Poseidon.EntityTypes

import qualified Web.Scotty as S
import Text.Blaze.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Data.Version (showVersion, Version)

renderMaybeVersion :: Maybe Version -> H.AttributeValue
renderMaybeVersion Nothing  = H.toValue ("" :: String)
renderMaybeVersion (Just v) = H.toValue $ showVersion v

mainPage :: [PoseidonPackage] -> S.ActionM ()
mainPage pacs = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 "Poseidon packages"
      H.ul $ mapM_ (\pac -> H.li $ H.div $ do
              let pacName = getPacName pac
                  pacVersion = getPacVersion pac
                  pacNameAndVersion = renderNameWithVersion $ posPacNameAndVersion pac
              H.a H.! HA.href ("/package/" <> H.toValue pacName <> "?package_version=" <> renderMaybeVersion pacVersion) $
                H.toMarkup pacNameAndVersion
              H.toMarkup (" | " :: String)
              H.a H.! HA.href ("/zip_file/" <> H.toValue pacName <> "?package_version=" <> renderMaybeVersion pacVersion) $
                H.toMarkup ("Download" :: String)
          ) pacs

packagePage :: PoseidonPackage -> S.ActionM ()
packagePage pac = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 (H.toMarkup $ renderNameWithVersion $ posPacNameAndVersion pac)