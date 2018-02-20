module Easy where

import Prelude ((*))

import Math (pi) -- from purescript-math package

circleArea :: Number -> Number
circleArea r = pi * r * r