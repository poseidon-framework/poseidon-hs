module Poseidon.MathHelpers where

import          Data.List        (foldl')

-- | A helper function to calculate the mean of a list of doubles
avg :: [Double] -> Double
--avg [] = 0/0
avg xs = let sum_ = foldl' (+) 0 xs
         in sum_ / fromIntegral (length xs)

-- | A helper function to round doubles
roundTo :: Int -> Double -> Double
roundTo n x = fromIntegral val / t
  where
    val = floor (x * t) :: Int
    t = 10^n

-- | A helper function to calculate the standard deviation of a list of doubles
stdev :: [Double] -> Double
stdev xs = sqrt . avg . map ((^2) . (-) (avg xs)) $ xs

-- | A helper function to get a nice string with mean and sd for a list of doubles
meanAndSdRoundTo :: Int -> [Double] -> String
meanAndSdRoundTo _ [] = "no values"
meanAndSdRoundTo n xs = show (roundTo n $ avg xs) ++ " ± " ++ show (roundTo n $ stdev xs)

-- | A helper function to get a nice string with mean and sd for a list of doubles
-- (here rounded to integer)
meanAndSdInteger :: [Double] -> String
meanAndSdInteger [] = "no values"
meanAndSdInteger xs = show (round $ avg xs) ++ " ± " ++ show (round $ stdev xs)