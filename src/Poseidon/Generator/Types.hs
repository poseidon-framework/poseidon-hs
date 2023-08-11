module Poseidon.Generator.Types where

import           Data.List      (intercalate)
import           Poseidon.Janno

data IndConcrete = IndConcrete {
      _indName   :: String
    , _groupName :: String
    , _popSet    :: [PopFracConcrete]
} deriving (Show)

data PopFracConcrete = PopFracConcrete {
      _popName :: String
    , _popFrac :: Rational
    , _popInds :: [(String, Int)]
} deriving (Show)

data RequestedInd = RequestedInd {
      _inIndName   :: String
    , _inGroupName :: String
    , _inPopSet    :: [PopFrac]
}

instance Show RequestedInd where
    show (RequestedInd _admixInd _admixUnit _popFracList) =
        "[" ++ _admixInd ++ ":" ++ _admixUnit ++ "]" ++
        "(" ++ intercalate "+" (map show _popFracList) ++ ")"

data PopFrac = PopFrac {
      _inPopName :: String
    , _inPopFrac :: Rational
}

instance Show PopFrac where
    show (PopFrac _pop _frac) =
        _pop ++ "=" ++ show _frac

data IndWithPosition = IndWithPosition {
      spatInd  :: String
    , spatUnit :: String
    , spatPos  :: SpatialTemporalPosition
} deriving (Show)

data SpatialTemporalPosition = SpatialTemporalPosition {
      time :: Int
    , lat  :: Latitude
    , lon  :: Longitude
} deriving (Show)
