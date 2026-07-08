{-# LANGUAGE TemplateHaskell #-}

module Poseidon.Core.ServerStylesheet where

import           Data.ByteString.Lazy as BL
import qualified Data.FileEmbed       as FE
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE

picocssBS :: BL.ByteString
picocssBS = BL.fromStrict $(FE.embedFile "data/pico.classless.blue.min.css")

htmlApiTableBS :: BL.ByteString
htmlApiTableBS = BL.fromStrict $(FE.embedFile "data/htmlapi-table.js")

htmlApiPlotsBS :: BL.ByteString
htmlApiPlotsBS = BL.fromStrict $(FE.embedFile "data/htmlapi-plots.js")

cssText :: T.Text
cssText = TE.decodeUtf8 $(FE.embedFile "data/htmlapi.css")
