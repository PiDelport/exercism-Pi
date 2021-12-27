module LeapYear (isLeapYear) where

-- | True if a year is a leap year.
isLeapYear :: Integral a => a -> Bool
isLeapYear =
  (`divisibleBy` 4)
    `unless` (`divisibleBy` 100)
    `unless` (`divisibleBy` 400)

-- True if @d@ divides @n@.
divisibleBy :: Integral a => a -> a -> Bool
n `divisibleBy` d = (n `mod` d) == 0

-- True if predicate @p@ holds, and @q@ does not.
--
-- When used as an infix operator, 'unless' is right-associative, so that
-- @p \`unless\` q \`unless\` r@ reads like English. (That is, @q@ is an
-- exception to @p@, and @r@ is an exception to @q@.)
infixr 9 `unless`
unless :: (t -> Bool) -> (t -> Bool) -> t -> Bool
(p `unless` q) x = p x && not (q x)
