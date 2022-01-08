module Grains (square, total) where

import Control.Applicative (Alternative, empty)

-- | The number of grains on a single square of the wheat and chessboard problem.
-- Defined for /1 <= n <= 64/.
square :: (Alternative f, Integral n, Num m) => n -> f m
square n =
  (2 ^ (n - 1))
    `onlyIf` inRange (1, 64) n

-- | The number of grains on the entire board of the wheat and chessboard problem.
total :: Num a => a
total = mersenne (64 :: Integer)

-- | The /n/th Mersenne number.
mersenne :: (Integral n, Num m) => n -> m
mersenne n = 2 ^ n - 1

-- * Generic helpers

-- | Lift a value if and only if a condition is true.
onlyIf :: Alternative f => a -> Bool -> f a
x `onlyIf` condition
  | condition = pure x
  | otherwise = empty

-- | Like 'Data.Ix.inRange', but for any 'Ord'.
inRange :: Ord a => (a, a) -> a -> Bool
inRange (lower, upper) n = lower <= n && n <= upper
