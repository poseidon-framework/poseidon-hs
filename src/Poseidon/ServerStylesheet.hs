{-# LANGUAGE TemplateHaskell #-}

module Poseidon.ServerStylesheet (stylesBS) where

import           Data.ByteString.Lazy as BL
import qualified Data.FileEmbed       as FE

stylesBS :: BL.ByteString
stylesBS = BL.fromStrict $(FE.embedFile "data/pico.classless.blue.min.css")
