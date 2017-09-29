module HaskellForAll.FunctionalMonoid() where

import Prelude hiding (mempty, mappend)
-- From  Monoids: Theme and Variations (Functional Pearl)

class Monoid' a where
  mempty :: a
  mappend :: a -> a -> a

instance Monoid' m => Monoid' (a -> m) where
  mempty = const mempty
  f `mappend` g = \x -> f x `mappend` g x


hom :: Monoid m => (a -> m) -> [a] -> m
hom f = mconcat . fmap f
