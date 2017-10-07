{-# LANGUAGE RankNTypes #-}
module Lens.LensOverTea() where

import Data.Bifunctor
import Control.Monad(liftM)
import Data.Functor.Compose
import Data.Functor.Identity


-- This is needed so that we can have constraints in type synonyms.



-- From https://artyom.me/lens-over-tea-1

-- is (->) a bifunctor? ( I think no, parm is contravariant?)
-- Is it really necessary to explicitly return the original value?
-- Meet Storey, functor modifier const * functor
type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a =  forall f .Functor f => (a-> f a) -> s -> f s

data Storey x f a = Storey x (f a)
  deriving Show

instance Functor f => Functor (Storey x f) where
  fmap f (Storey x fa) = Storey x (fmap f fa)


-- ix :: Int -> (a -> a) -> [a] -> (a, [a])
ix :: Int -> Lens' [a] a
ix index f list
  | index < 0        = error "ix: negative index"
  | null list        = error "ix: index too large"
  | old:rest <- list = if index == 0
                         then  (: rest) <$> (f old)
                         else (old:) <$> ix (index-1) f rest



over :: Lens s t a b -> ((a -> b) -> s -> t)
over l f = runIdentity . l (Identity . f)

-- TODO: other functions view, ...
-- TODO: Test yourself...
