{-# LANGUAGE TemplateHaskell #-}

module Poseidon.Core.ServerStylesheet where

import           Data.ByteString.Lazy as BL
import qualified Data.FileEmbed       as FE
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

stylesBS :: BL.ByteString
stylesBS = BL.fromStrict $(FE.embedFile "data/pico.classless.blue.min.css")

jsText :: T.Text
jsText = TE.decodeUtf8 $(FE.embedFile "data/htmlapi.js")
