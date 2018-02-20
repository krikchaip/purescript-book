module Chapter08 where

import Prelude

import Control.Monad.Eff (Eff, forE)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.ST (modifySTRef, newSTRef, readSTRef, runST)
import Data.Array (tail, head, foldM, sort)
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))

-------------------- Easy --------------------
third :: forall a. Array a -> Maybe a
third xs = do
  xs'    <- tail xs
  xs''   <- tail xs'
  head xs''
----------------------------------------------

------------------- Medium -------------------
sums :: Array Int -> Array Int
sums = sort <<< foldM (\acc coin -> [acc, acc + coin]) 0
----------------------------------------------

------------------- Medium -------------------
prove1 :: Maybe Int
prove1 = (+) <$> Just 1 <*> Just 2

prove1' :: Maybe Int
prove1' = ((+) <$> Just 1) `ap` Just 2
----------------------------------------------

------------------- Medium -------------------
-- Left Identity: `pure x >>= f = f x`
prove2 :: Maybe Int
prove2 = pure 1 >>= \x -> Just (x `mod` 3)

prove2' :: Maybe Int
prove2' = (\x -> Just (x `mod` 3)) 1

-- Right Identity: `m >>= pure = m`
prove3 :: Maybe Int
prove3 = Just 1 >>= pure

prove3' :: Maybe Int
prove3' = Just 1
----------------------------------------------

------------------- Medium -------------------
filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (a : as) = do
  b <- f a
  if b
    then (:) <$> pure a <*> filterM f as
    else filterM f as
----------------------------------------------

------------------- Medium -------------------
safeDivide ::
  forall eff.
  Number ->
  Number ->
  Eff (exception :: EXCEPTION | eff) Number
safeDivide _ 0.0 = throw "the denominator is zero"
safeDivide a b = pure (a / b)
----------------------------------------------

------------------ Difficult -----------------
estimatePI :: forall eff. Int -> Eff (random :: RANDOM | eff) Number
estimatePI iteration = runST do
  inside <- newSTRef 0.0
  forE 0 iteration \_ -> void $ do
    x <- random
    y <- random
    if (x * x) + (y * y) < 1.0
      then modifySTRef inside (_ + 1.0)
      else modifySTRef inside id
  total <- readSTRef inside
  pure (4.0 * total / (toNumber iteration))
----------------------------------------------