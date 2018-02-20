module Easy where

import Prelude (otherwise, ($), (&&), (*), (+), (-), (<), (<$>), (==), (>=), (>>>))

import Data.Array
import Playground (factors')

-- ex01
isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n
  | n < 0 = isEven $ n + 2
  | otherwise = isEven $ n - 2

-- ex02
squares :: Array Number -> Array Number
squares = (<$>) (\n -> n * n)

-- ex03
removeNeg :: Array Number -> Array Number
removeNeg = filter (\n -> n >= 0.0)

-- ex04
isPrime :: Int -> Boolean
isPrime = factors' >>> length >>> (==) 1

-- ex05
alltrue :: Array Boolean -> Boolean
alltrue = foldl (&&) true
