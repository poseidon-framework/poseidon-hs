{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Core.ContributorSpec (spec) where

import           Poseidon.Core.Contributor

import           Data.Either               (isLeft)
import           Test.Hspec
import qualified Text.Parsec               as P
import           Text.Parsec.Error         (Message (..), newErrorMessage)
import           Text.Parsec.Pos           (newPos)

spec :: Spec
spec = do
    testContributorSpecParser

testContributorSpecParser :: Spec
testContributorSpecParser =
    describe "Poseidon.Core.Contributor.contributorSpecParser" $ do
    it "should work for one individual" $ do
        P.runParser contributorSpecParser () "" "[A B C](abc@hmail.com)" `shouldBe`
            Right [ContributorSpec "A B C" "abc@hmail.com" Nothing]
    it "should work for multiple individuals" $ do
            P.runParser contributorSpecParser () "" "[A B C](abc@hmail.com);[D E F](def@hmail.com)" `shouldBe`
                Right [ContributorSpec "A B C" "abc@hmail.com" Nothing,
                       ContributorSpec "D E F" "def@hmail.com" Nothing]
    it "should work with ORCIDs" $ do
            P.runParser contributorSpecParser () ""
                "[A B C](abc@hmail.com)<0000-0002-1825-0097>;[D E F](def@hmail.com)<0000-0003-3448-5715>" `shouldBe`
                Right [ContributorSpec "A B C" "abc@hmail.com" (Just $ ORCID "000000021825009" '7'),
                       ContributorSpec "D E F" "def@hmail.com" (Just $ ORCID "000000033448571" '5')]
    it "should fail with wrong ORCIDs" $ do
            P.runParser contributorSpecParser () "" "[A B C](abc@hmail.com)<0000-0003-3448-5716>;[D E F](def@hmail.com)" `shouldBe`
                Left (newErrorMessage (Expect "ORCID is not valid") (newPos "" 1 43))
    it "should fail with trailing garbage" $ do
            P.runParser contributorSpecParser () "" "[A B C](abc@hmail.com)Test;[D E F](def@hmail.com)" `shouldSatisfy` isLeft
            P.runParser contributorSpecParser () "" "[A B C](abc@hmail.com);[D E F](def@hmail.com)Test" `shouldSatisfy` isLeft
