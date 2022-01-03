module SumOfMultiples (sumOfMultiples, sumOfMultiplesConstructive, sumOfMultiplesAnalytical) where

import Data.List (foldl', subsequences)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples = sumOfMultiplesAnalytical

-- * Approach 1: Constructive solution

-- | Merge two strictly ascending lists, eliminating duplicates.
mergeUniqAsc :: Ord a => [a] -> [a] -> [a]
mergeUniqAsc xxs@(x : xs) yys@(y : ys) =
  case compare x y of
    LT -> x : mergeUniqAsc xs yys -- pull from xxs
    EQ -> x : mergeUniqAsc xs ys -- pull from both
    GT -> y : mergeUniqAsc xxs ys -- pull from yys
mergeUniqAsc xs [] = xs
mergeUniqAsc [] ys = ys

-- | Merge a finite collection of strictly ascending lists.
mergeUniqAscMany :: Ord a => [[a]] -> [a]
mergeUniqAscMany = foldl' mergeUniqAsc []

-- | List the multiples of a factor.
multiples :: Integral a => a -> [a]
multiples 0 = [0] -- Special case: zero's only multiple is zero.
multiples factor = [n * factor | n <- [0 ..]]

-- | List the unique, ascending multiples of a collection of non-negative factors.
multiplesUniqAsc :: Integral a => [a] -> [a]
multiplesUniqAsc factors
  | all (0 <=) factors = mergeUniqAscMany (multiples <$> factors)
  | otherwise = error "negative factors not supported"

-- | Sum all unique multiples of the given factors, up to the given limit.
-- This implementation lists and sums the factors, with cost proportional to the limit.
sumOfMultiplesConstructive :: Integral a => [a] -> a -> a
sumOfMultiplesConstructive factors limit =
  sum . takeWhile (< limit) . multiplesUniqAsc $ factors

-- * Approach 2: Analytical solution

-- | Return the /n/th triangle number.
triangle :: Integral a => a -> a
triangle n = n * (n + 1) `div` 2

-- | The `lcm` of a list.
lcmList :: Integral a => [a] -> a
lcmList = foldl' lcm 1

-- | Sum the multiples of the given number, up to the given limit.
sumOfMultiplesOfOne :: Integral a => a -> a -> a
sumOfMultiplesOfOne 0 _ = 0 -- Special case: zero's only multiple is zero.
sumOfMultiplesOfOne n limit = n * triangle ((limit - 1) `div` n)

-- | Sum all unique multiples of the given factors, up to the given limit.
-- This implementation calculates the sum with cost proportional to the number of factors.
sumOfMultiplesAnalytical :: Integral a => [a] -> a -> a
sumOfMultiplesAnalytical factors limit =
  -- Sum the contribution of each non-empty subset of the given factors.
  sum
    [ contribution subfactors
      | subfactors <- subsequences factors,
        not (null subfactors)
    ]
  where
    -- The contribution of each subset of factors is the sum of the multiples of the subset's LCM,
    -- counted as positive for odd-sized subsets, and negative for even-sized subsets.
    -- (That is, each cardinality of subsets corrects for the over-counting of the previous smaller cardinality.)
    contribution subfactors =
      polarity * sumOfMultiplesOfOne (lcmList subfactors) limit
      where
        polarity = if odd (length subfactors) then 1 else -1
