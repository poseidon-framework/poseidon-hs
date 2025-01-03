{-# LANGUAGE OverloadedStrings #-}

module Poseidon.ServerHTML (mainPage, packagePage) where

import Poseidon.Package
import Poseidon.EntityTypes

import qualified Web.Scotty as S
import Text.Blaze.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H


mainPage :: [PoseidonPackage] -> S.ActionM ()
mainPage pacs = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 "Poseidon packages"
      H.ul $ mapM_ (\pac -> H.li $ H.div $ do
              let nameAndVersion = renderNameWithVersion $ posPacNameAndVersion pac
              H.a H.! H.href ("/package/" <> H.toValue nameAndVersion) $ H.toMarkup nameAndVersion
          ) pacs

packagePage :: PoseidonPackage -> S.ActionM ()
packagePage pac = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 (H.toMarkup $ renderNameWithVersion $ posPacNameAndVersion pac)