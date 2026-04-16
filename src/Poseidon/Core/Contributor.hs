{-# LANGUAGE OverloadedStrings #-}

module Poseidon.Contributor (
    ContributorSpec (..),
    contributorSpecParser,
    ORCID (..),
    renderORCID
) where

import           Poseidon.Utils     (showParsecErr)

import           Control.Monad      (guard, mzero)
import           Data.Aeson         (FromJSON, ToJSON (..), Value (String),
                                     object, parseJSON, toJSON, withObject,
                                     (.:), (.:?), (.=))
import           Data.Char          (digitToInt)
import           Data.List          (intercalate)
import           Data.Text          (pack, unpack)
import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P


-- | A data type to represent a contributor
data ContributorSpec = ContributorSpec
    { contributorName  :: String -- ^ the name of a contributor
    , contributorEmail :: String -- ^ the email address of a contributor
    , contributorORCID :: Maybe ORCID -- ^ the ORCID of a contributor
    }
    deriving (Show, Eq, Ord)

-- | To facilitate automatic parsing of ContributorSpec from JSON files
instance FromJSON ContributorSpec where
    parseJSON = withObject "contributor" $ \v -> ContributorSpec
        <$> v .:  "name"
        <*> v .:  "email"
        <*> v .:? "orcid"

instance ToJSON ContributorSpec where
    -- this encodes directly to a bytestring Builder
    toJSON x = object [
          "name"  .= contributorName x
        , "email" .= contributorEmail x
        , "orcid" .= contributorORCID x
        ]

contributorSpecParser :: P.Parser [ContributorSpec]
contributorSpecParser = P.try (P.sepBy oneContributorSpecParser (P.char ';' <* P.spaces))

oneContributorSpecParser :: P.Parser ContributorSpec
oneContributorSpecParser = do
    name <- P.between (P.char '[') (P.char ']') (P.manyTill P.anyChar (P.lookAhead (P.char ']')))
    email <- P.between (P.char '(') (P.char ')') (P.manyTill P.anyChar (P.lookAhead (P.char ')')))
    -- TODO: add option to add ORCID here
    return (ContributorSpec name email Nothing)

-- | A data type to represent an ORCID
-- see https://support.orcid.org/hc/en-us/articles/360006897674-Structure-of-the-ORCID-Identifier
data ORCID = ORCID
    { _orcidNums     :: [Char]
    , _orcidChecksum :: Char
    }
    deriving (Show, Eq, Ord)

instance FromJSON ORCID where
    parseJSON (String s) = case P.runParser parseORCID () "" (unpack s) of
        Left err -> fail $ showParsecErr err
        Right x  -> pure x
    parseJSON _          = mzero

instance ToJSON ORCID where
    toJSON x = String $ pack $ renderORCID x

parseORCID :: P.Parser ORCID
parseORCID = do
    orcid <- (\a b c d e -> ORCID (concat [a,b,c,d]) e) <$>
            fourBlock <* m
        <*> fourBlock <* m
        <*> fourBlock <* m
        <*> threeBlock <*> checksumDigit <* P.eof
    guard (validateORCID orcid) P.<?> "ORCID is not valid"
    return orcid
  where
      fourBlock = P.count 4 P.digit
      m = P.oneOf "-"
      threeBlock = P.count 3 P.digit
      checksumDigit = P.digit P.<|> P.char 'X'

validateORCID :: ORCID -> Bool
validateORCID (ORCID nums check) =
    let numsInt = map digitToInt nums
        total = makeTotal 0 numsInt
        remainder = total `mod` 11
        result = (12 - remainder) `mod` 11
        checkInt = if check == 'X' then 10 else digitToInt check
    in result == checkInt
    where
        makeTotal :: Int -> [Int] -> Int
        makeTotal a []     = a
        makeTotal a (x:xs) = makeTotal ((a + x) * 2) xs

renderORCID :: ORCID -> String
renderORCID (ORCID nums check) =
    intercalate "-" (chunks 4 nums) ++ [check]
    where
        chunks :: Int -> [a] -> [[a]]
        chunks _ [] = []
        chunks n xs =
            let (ys, zs) = splitAt n xs
            in  ys : chunks n zs
