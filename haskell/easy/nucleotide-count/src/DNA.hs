module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Text.Read (readEither)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

-- | Count the frequency of each nucleotide in the given string.
-- Return an error message for any invalid values.
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = histogram <.> traverse nucleotideFromChar

-- | Parse a 'Char' to a 'Nucleotide'.
nucleotideFromChar :: Char -> Either String Nucleotide
nucleotideFromChar c = readEither [c]

-- | Count the frequency of each value in a sequence.
histogram :: (Ord k, Num a) => [k] -> Map k a
histogram ns = Map.fromListWith (+) [(n, 1) | n <- ns]

-- | 'f . g' for a functor context.
(<.>) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(f <.> g) a = f <$> g a

infixr 9 <.>
