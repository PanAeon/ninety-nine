{-# LANGUAGE RankNTypes, TupleSections #-}
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
type Lens' s a =  Lens s s a a

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

-- Test yourself --


-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (,x) <$> f a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2  f (x, a) = (x,) <$> f a


-- Make a lens out of a getter and a setter.
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set f x = (set x) <$> (f (get x))


-- Combine 2 lenses to make a lens which works on Either. (It's a good idea
-- to try to use bimap for this, but it won't work, and you have to use
-- explicit case-matching. Still a good idea, tho.)
choosing :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 f s = case s of
                     Left  s1 -> Left <$> l1 f s1
                     Right s2 -> Right <$> l2 f s2  -- FIXME: what's that with bymap?

choosing' :: Lens s1 t1 a b -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing' l1 l2 f = either ((fmap Left) . l1 f) ((fmap Right ) . l2 f)



-- Modify the target of a lens and return the result. (Bonus points if you
-- do it without lambdas and defining new functions. There's also a hint
-- before the end of the section, so don't scroll if you don't want it.)

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = (l (\x -> (f x, f x)) s)

(<%~~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~~) l f s = (l ((,) <$> f <*> f) s)

-- Modify the target of a lens, but return the old value.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = (l ((,) <$> id <*> f) s)

-- There's a () in every value. (No idea what this one is for, maybe it'll
-- become clear later.)
-- (a -> f b) -> s -> f t
--  (() -> f ()) -> s -> f s
united :: Lens' s ()
united g s =  const s <$> (g ())



-- TODO: other functions view, ...
