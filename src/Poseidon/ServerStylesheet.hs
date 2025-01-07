{-# LANGUAGE TemplateHaskell #-}

module Poseidon.ServerStylesheet (stylesBS) where

import qualified Data.FileEmbed            as FE
import Data.ByteString.Lazy as BL

stylesBS :: BL.ByteString
stylesBS = BL.fromStrict $(FE.embedFile "data/pico.classless.blue.min.css")
