module Grains (square, total) where

import Control.Applicative (Alternative, empty)

-- | Lift a number if it's a valid "chessboard square" number (/1 <= n <= 64/).
asChessboardNumber :: (Ord a, Num a, Alternative f) => a -> f a
asChessboardNumber n
  | 1 <= n && n <= 64 = pure n
  | otherwise = empty

-- | The number of grains on a single square of the wheat and chessboard problem.
-- Defined for valid 'asChessboardNumber' inputs.
square :: (Alternative f, Integral n, Num m) => n -> f m
square = square' <.> asChessboardNumber
  where
    square' n = 2 ^ (n - 1)

-- | The number of grains on the entire board of the wheat and chessboard problem.
total :: Num a => a
total = mersenne (64 :: Integer)

-- | The /n/th Mersenne number.
mersenne :: (Integral n, Num m) => n -> m
mersenne n = 2 ^ n - 1

-- * Generic helpers

-- | 'f . g' for a functor context.
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) a = f <$> g a

infixr 9 <.>
