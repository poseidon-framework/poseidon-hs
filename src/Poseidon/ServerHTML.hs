{-# LANGUAGE OverloadedStrings #-}

module Poseidon.ServerHTML (mainPage) where

import Poseidon.Package
import Poseidon.EntityTypes

import qualified Web.Scotty as S
import Text.Blaze.Renderer.Text
import qualified Text.Blaze.Html5 as H


mainPage :: [PoseidonPackage] -> S.ActionM ()
mainPage pacs = S.html $ renderMarkup $ do
  H.html $ do
    H.body $ do
      H.h1 "Poseidon packages"
      H.ul $ mapM_ (\pac -> H.li $ H.div $ do
              H.toMarkup (renderNameWithVersion $ posPacNameAndVersion pac)
          ) pacs
