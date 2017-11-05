{-# LANGUAGE RankNTypes, TupleSections,MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}
module Lens.LensOverTea() where

import Data.Bifunctor
import Control.Monad(liftM)
import Data.Functor.Compose
import Data.Functor.Identity
import Control.Applicative(Const(..))
import Data.Monoid(Any(..), First(..))


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



--over :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
over :: Setting s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

--view :: ((a -> Const a a) -> s -> Const a s) -> s -> a
view :: Getting s a -> s -> a
view l = getConst . l Const

--set :: ((a -> Identity b) -> s -> Identity t) -> b -> s -> t
set :: Setting s t a b -> b -> s -> t
set l x = runIdentity . l (Identity . (const x))
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

toListOf :: ((a -> Const [a] a) -> s -> Const [a] s) -> s -> [a]
toListOf l = getConst . l (\x -> Const [x])


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

_abs :: Real a => Lens' a a
_abs f n = update <$> f (abs n)
  where
    update x
      | x < 0 = error "abs(x) could not be negative"
      | otherwise = signum n * x

-- _all :: Eq a => a -> Lens' [a] a
-- _all x f xs
--   | null xs        =  fmap id xs
--   | old:rest <- xs = if x == old
--                          then  (: rest) <$> (f old)
--                          else (old:) <$> _all x f rest

_all'' :: Eq a => a -> Lens' [a] a
_all'' ref = lens get set
  where
    get s     = ref
    set s new = map (\old -> if old == ref then new else old) s

type AppLens s t a b =forall f . Applicative f => (a -> f b) -> s -> f t
type AppLens' s a = AppLens s s a a


_all' :: Eq a => a -> AppLens' [a] a
_all' ref f s = traverse update s
  where
    update old = if old == ref then f old else pure old

type Getting s a = (a -> Const a a) -> s -> Const a s

type Setting s t a b = (a -> Identity b) -> s -> Identity t

preview
  :: ((a -> Const (First a) a) -> s -> Const (First a) s)
  -> s
  -> Maybe a
preview l = getFirst . getConst . l (\x -> Const (First (Just x)))

has:: ((a -> Const Any a) -> s -> Const Any s) -> s -> Bool
has l = getAny . getConst . l (\x -> Const (Any True))

class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

type Traversal s t a b = forall f . Applicative f => (a -> f b) -> s -> f t
type Traversal' s a = Traversal s s a a

--instance T.Traversable t => Each (t a) (t b) a b where
--  each = T.traverse
