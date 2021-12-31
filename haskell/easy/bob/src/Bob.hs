{-# LANGUAGE OverloadedStrings #-}

module Bob (responseFor) where

import Data.Char (isAlpha, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

-- | Bob's conversational categories.
data Category = Question | Yelled | YelledQuestion | Silence | Other

-- | Bob's response for the given input.
responseFor :: Text -> Text
responseFor xs = case categorise xs of
  Question -> "Sure."
  Yelled -> "Whoa, chill out!"
  YelledQuestion -> "Calm down, I know what I'm doing!"
  Silence -> "Fine. Be that way!"
  Other -> "Whatever."

-- | Categorise the given input, according to Bob.
categorise :: Text -> Category
categorise input
  | question && yelling = YelledQuestion
  | question = Question
  | yelling = Yelled
  | T.null stripped = Silence
  | otherwise = Other
  where
    stripped = T.strip input
    question = "?" `T.isSuffixOf` stripped
    yelling = not (T.null letters) && T.all isUpper letters
    letters = T.filter isAlpha stripped
