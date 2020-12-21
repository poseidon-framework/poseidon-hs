module Poseidon.MathHelpersSpec (spec) where

import           Poseidon.MathHelpers

import           Data.Vector            (fromList)
import           Statistics.Sample      (mean)
import           Test.Hspec
import           Test.QuickCheck

spec = do
    testAvg

testAvg :: Spec
testAvg = 
    describe "Poseidon.MathHelpers.avg" $ do
    it "should always be within the range of the input list" $ property test
        where 
            test :: [Double] -> Bool
            test [] = True 
            test x = minimum x <= avg x && maximum x >= avg x
                       

