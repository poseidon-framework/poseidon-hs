module Poseidon.Analysis.FStatsSpec (spec) where

import           Poseidon.Analysis.CLI.FStats   (collectStatSpecGroups)
import           Poseidon.Analysis.FStatsConfig (AscertainmentSpec (..),
                                                 FStatSpec (..), FStatType (..))
import           Poseidon.EntitiesList          (PoseidonEntity (..),
                                                 PoseidonIndividual (..))

import           Test.Hspec

spec :: Spec
spec = do
    testCollectStats

testCollectStats :: Spec
testCollectStats = describe "collectStatSpecGroups" $ do
    it "should correctly collect stats" $ do
        let statSpecs = [
                FStatSpec F3 [Group "French", Group "Spanish", Ind (SimpleInd "Chimp.REF")] Nothing,
                FStatSpec F3 [Group "French", Group "Mbuti", Ind (SimpleInd "Chimp.REF")] (Just (AscertainmentSpec (Just (Ind (SimpleInd "Human.REF"))) (Group "CEU") 0.05 0.95))]
            entities = [Group "French", Group "Spanish", Ind (SimpleInd "Chimp.REF"), Group "Mbuti", Ind (SimpleInd "Human.REF"), Group "CEU"]
        collectStatSpecGroups statSpecs `shouldMatchList` entities
