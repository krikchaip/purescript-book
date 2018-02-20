module Part1 where

import Prelude

import Data.Array ((..))

-- easy
factorial :: Int -> Int
factorial 0 = 1
factorial n = let
  recursive acc 1 = acc
  recursive acc x = recursive (acc * x) (x - 1)
  in recursive 1 n

-- medium
pascal :: Int -> Array Int
pascal n = (n `choose` _) <$> 0 .. n where
  choose n r = factorial n / (factorial r * factorial (n - r))