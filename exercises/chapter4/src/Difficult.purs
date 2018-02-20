module Difficult where

import Prelude
import Data.Array
import Data.Maybe

factorizations :: Int -> Array Int
factorizations 1 = [1]
factorizations 2 = [2]
factorizations n = case factor of
  Just f -> f : factorizations (n / f)
  Nothing -> [n]
  where factor = filter (\x -> n `mod` x == 0) >>> head $ 2..(n-1)