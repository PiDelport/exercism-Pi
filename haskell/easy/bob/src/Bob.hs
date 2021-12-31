module Bob (responseFor) where

import Data.Char (isAlpha, isSpace, isUpper)
import Data.List (dropWhileEnd)

-- | Bob's response for the given input.
responseFor :: String -> String
responseFor xs
  | question && yelling = "Calm down, I know what I'm doing!"
  | question = "Sure."
  | yelling = "Whoa, chill out!"
  | nothing = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    text = stripSpace xs
    nothing = null text
    question = not nothing && last text == '?'
    yelling = not (null letters) && all isUpper letters
    letters = filter isAlpha text

-- | Strip leading and trailing whitespace from a string.
stripSpace :: String -> String
stripSpace = dropWhileEnd isSpace . dropWhile isSpace
