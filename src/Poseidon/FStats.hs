module Poseidon.FStats where

data FStatSpec = F4Spec GroupSpec GroupSpec GroupSpec GroupSpec |
  F3Spec GroupSpec GroupSpec GroupSpec |
  F2Spec GroupSpec GroupSpec |
  PairwiseMismatchSpec GroupSpec GroupSpec

type GroupSpec = [GroupElementSpec]

data GroupElementSpec = GroupName String | IndName String

