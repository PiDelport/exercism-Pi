module CollatzConjecture (collatz) where

import Data.List (genericLength)

-- | The length of the Collatz sequence starting at n, for positive n.
-- Terminates if the Collatz conjecture is true. 
collatz :: Integer -> Maybe Integer
collatz n
  | 0 < n = Just (genericLength . takeWhile (/= 1) $ collatzSequence n)
  | otherwise = Nothing

-- | The Collatz sequence starting at the given integer.
collatzSequence :: Integral a => a -> [a]
collatzSequence = iterate step
  where
    step n
      | even n = n `div` 2
      | otherwise = 3 * n + 1
