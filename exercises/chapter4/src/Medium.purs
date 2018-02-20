module Medium where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, foldl, (..), (:))
import Data.Array.Partial (head, tail)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

-- ex01
countEven :: Array Int -> Int
countEven [] = 0
countEven xs
  | unsafePartial head xs `mod` 2 == 0 = 1 + countEven (unsafePartial tail xs)
  | otherwise = countEven $ unsafePartial tail xs

-- ex02
infixr 0 filter as <$?>
removeNeg' :: Array Number -> Array Number
removeNeg' = (<$?>) (\n -> n >= 0.0)
-- ถ้ามีหลายเครื่องหมายอยู่ใน expression เดียวกัน -> ทำอันที่ precedence สูงกว่าก่อน
-- ถ้ามีเครื่องหมายเดียวกันหลายอันใน expression -> ทำตาม associativity
-- ถ้า precedence เท่ากัน และ/หรือ ไม่ได้กำหนด associativity -> ทำจากซ้ายไปขวา
-- (\n -> n >= 3) <$?> (\n -> n /= 4) <$?> (\n -> n + 1) <$> [1,2,3,4] :: [3,5]

-- ex03
x :: forall a b. Array a -> Array b -> Array (Tuple a b)
x as bs = do
  a <- as
  b <- bs
  pure $ Tuple a b

-- ex04
triples :: Int -> Array (Array Int)
triples n = do
  a <- (1..n)
  b <- (1..n)
  c <- (1..n)
  guard $ (a * a) + (b * b) == (c * c)
  pure [a, b, c]

-- ex05
-- [false, false, false, ...]

-- ex06
countif :: forall a. (a -> Boolean) -> Array a -> Int
countif = self 0 where
  self acc _ [] = acc
  self acc f xs
    | f $ unsafePartial head xs = self (acc + 1) f (unsafePartial tail xs)
    | otherwise = self acc f (unsafePartial tail xs)

-- ex07
reverse :: forall a. Array a -> Array a
reverse = foldl (\acc x -> x : acc) []