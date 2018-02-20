module Chapter06 where

import Prelude

import Data.Foldable (class Foldable, foldMap, foldl, foldr, maximum)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)

-------------------- easy --------------------
newtype Complex = Complex
  { real :: Number
  , imaginary :: Number}

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) =
    "(" <> show real <> ", " <> show imaginary <> "i)"

instance eqComplex :: Eq Complex where
  eq (Complex { real: a1, imaginary: b1 })
     (Complex { real: a2, imaginary: b2 }) = a1 == a2 && b1 == b2
----------------------------------------------

-------------------- easy --------------------
data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys
----------------------------------------------

------------------- medium -------------------
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> ys)
----------------------------------------------

------------------- medium -------------------
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)
----------------------------------------------

------------------- medium -------------------
data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq (Finite a) (Finite b) = a == b
  eq _ _ = false

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare (Finite a) (Finite b) = compare a b
  compare Infinite _ = GT
  compare _ Infinite = LT
----------------------------------------------

------------------- medium -------------------
arrmax :: Partial => Array Int -> Int
arrmax xs = x where Just x = maximum xs
----------------------------------------------

------------------- medium -------------------
newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

class Monoid m <= Action m a where
  act :: m -> a -> a
  -- act mempty a = a
  -- act (m1 <> m2) a = act m1 (act m2 a)

instance repeatAction :: Action Multiply String where
  act (Multiply n) s
    | n > 0 = s <> act (Multiply (n - 1)) s
    | otherwise = ""

-- test = act (Multiply 4) "Winner"
-- test2 = act (Multiply 4 <> Multiply 2) "Winner"
-- test3 = act (mempty :: Multiply) "Winner"
----------------------------------------------

------------------- medium -------------------
instance actionOnArray :: Action m a => Action m (Array a) where
  act m xs = xs <#> act m

-- test = act (Multiply 3) ["a", "b", "c"]
-- test2 = act (Multiply 3 <> Multiply 2) ["a", "b", "c"]
-- test3 = act (Multiply 3) $ act (Multiply 2) ["a", "b", "c"]
----------------------------------------------

------------------ difficult -----------------
instance foldableNonEmpty :: Foldable NonEmpty where
  foldl f acc (NonEmpty x []) = foldl f acc [x]
  foldl f acc (NonEmpty _ xs) = foldl f acc xs
  foldr f acc (NonEmpty x []) = foldr f acc [x]
  foldr f acc (NonEmpty _ xs) = foldr f acc xs
  foldMap f (NonEmpty x []) = f x
  foldMap f (NonEmpty _ xs) = foldMap f xs
----------------------------------------------

------------------ difficult -----------------
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldr f acc (OneMore x xs) = foldr f (f x acc) xs
  foldMap f (OneMore x xs) = f x <> foldMap f xs
----------------------------------------------

------------------ difficult -----------------
newtype Self m = Self m

instance selfAction :: (Monoid m) => Action m (Self m) where
  act m1 (Self m2) = Self (m2 <> m1)
----------------------------------------------

------------------ difficult -----------------
-- ไม่จำเป็นต้องใช้ functional dependency เพราะว่า
-- m, a ไม่มีความจำเป็นต้องเกี่ยวข้องกันเสมอไป
----------------------------------------------