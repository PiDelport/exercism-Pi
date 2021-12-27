module Pangram (isPangram) where

import Data.Char (toLower)
import Data.List ((\\))

-- | True if a string contains all letters of the ASCII alphabet, case-insensitively.
isPangram :: String -> Bool
isPangram text = null $ alphabet \\ textChars
  where
    alphabet = ['a' .. 'z']
    textChars = toLower <$> text
