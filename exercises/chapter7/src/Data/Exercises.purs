module Chapter07 where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)

-------------------- easy --------------------
optPlus :: Maybe Number -> Maybe Number -> Maybe Number
optPlus = lift2 (+)

optSub :: Maybe Number -> Maybe Number -> Maybe Number
optSub = lift2 (-)

optMult :: Maybe Number -> Maybe Number -> Maybe Number
optMult = lift2 (*)

optDiv :: Maybe Number -> Maybe Number -> Maybe Number
optDiv = lift2 (/)
----------------------------------------------

------------------- medium -------------------
data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance functorTree :: Functor Tree where
  map f Leaf = Leaf
  map f (Branch l x r) = Branch (map f l) (f x) (map f r)

instance foldableTree :: Foldable Tree where
  foldl _ acc Leaf = acc
  foldl f acc (Branch Leaf x Leaf) = acc `f` x
  foldl f acc (Branch l x r) = foldl f (foldl f (acc `f` x) l) r

  foldr _ acc Leaf = acc
  foldr f acc (Branch Leaf x Leaf) = x `f` acc
  foldr f acc (Branch l x r) = foldr f (foldr f (x `f` acc) r) l

  foldMap _ Leaf = mempty
  foldMap f (Branch l x r) = foldMap f l <> f x <> foldMap f r

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch l x r) = Branch
    <$> traverse f l
    <*> f x
    <*> traverse f r
  sequence Leaf = pure Leaf
  sequence (Branch l m r) = Branch
    <$> sequence l
    <*> m
    <*> sequence r
----------------------------------------------

------------------ difficult -----------------
combineMaybe :: forall a f. Applicative f =>
  Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just f) = pure <$> f

-- test = combineMaybe (Just ['a', 'b', 'c'])
----------------------------------------------