module Poseidon.FStatsSpec (spec) where

import           Poseidon.FStats (FStatSpec (..), PopSpec (..), fStatSpecParser,
                                  runParser)
import           Test.Hspec

spec = do
    testParseStatSpec

testParseStatSpec :: Spec
testParseStatSpec = describe "Poseidon.FStatsSpec.fStatSpecParser" $ do
    it "should parse F4 correctly" $
        let f = F4Spec (PopSpecGroup "aa") (PopSpecInd "bb") (PopSpecGroup "cc") (PopSpecGroup "dd")
        in  runParser fStatSpecParser () "" "F4(aa,<bb>,cc,dd)" `shouldBe` Right f


