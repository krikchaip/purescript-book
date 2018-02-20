module Playground where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concatMap, (..), filter)
import Data.Foldable (product)

choose2P :: Int -> Array (Array Int)
choose2P n = concatMap (\i -> map (\j -> [i, j]) (1..n)) (1..n)
-- choose2 3 === [1, 2, 3] ->
-- [[[1, 1], [1, 2], [1, 3]], [[2, 1], [2, 2], [2, 3]], ...] ->
-- [[1, 1], [1, 2], [1, 3], [2, 1], [2, 2], ...]

choose2C :: Int -> Array (Array Int)
choose2C n = concatMap (\i -> map (\j -> [i, j]) (i..n)) (1..n)

factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $ -- do
  1..n >>= \i -> -- i <- 1..n
  1..n >>= \j -> -- j <- 1..n
  pure [i, j] -- [[i, j]]

factors' :: Int -> Array (Array Int)
factors' n = do
  i <- 1..n
  j <- i..n
  guard $ i * j == n -- if i * j == n then [unit] else []
  -- ([unit] >>= \_ -> ...) or ([] >>= \_ -> ...)
  pure [i, j]