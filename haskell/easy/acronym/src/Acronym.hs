{-# LANGUAGE OverloadedStrings #-}

module Acronym (abbreviate) where

import Data.Char (isLetter, isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

-- | Convert a phrase to its acronym.
abbreviate :: Text -> Text
abbreviate = T.toUpper . filterWithPrev isWordStart ' '

-- | True if a character boundary starts a word.
isWordStart :: Char -> Char -> Bool
isWordStart l r = (word && not apostraphe) || camelCase
  where
    word = not (isLetter l) && isLetter r
    apostraphe = [l, r] == "'s"
    camelCase = isLower l && isUpper r

-- | Like 'T.filter', but also provide the preceding character as context.
filterWithPrev :: (Char -> Char -> Bool) -> Char -> Text -> Text
filterWithPrev keep z = fst . T.foldl' f ("", z)
  where
    f (acc, l) r
      | keep l r = (acc `T.snoc` r, r)
      | otherwise = (acc, r)
