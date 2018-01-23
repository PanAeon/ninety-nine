module Cont.MyCont
    (
    ) where


import Control.Applicative(liftA2)

-- all right, so what's about those pesky cont monad?

-- I'm a bit fed up reading blogs. let's try to write my own mini blog.

-- Part I Simple Cont Monad
-- Points to consider:
-- * cont is a 'reification' of callbacks
-- * what is the *effect* of the cont monad?

-- let's write a simple callback method:
methodWithCallBack:: (a -> b) -> a -> b
methodWithCallBack callback a = undefined

methodReturnsCallback :: (a -> b)
methodReturnsCallback  = undefined

-- let's define a cont datatype (for simplicity I'll stick with a->b type)
-- don't remember why but traditionally it is smth like (() -> r),
-- without first param, but I'm doing it like a peasant
-- note that it is covariant (looks that to me)

data Cont a r = Cont (a->r)

runCont :: (Cont a r) -> a -> r
runCont (Cont g) a = g(a)

data Cont' a r = Cont' {
  runCont' :: a -> r
}

-- let's define functor for it
instance Functor (Cont a) where
  f `fmap` (Cont g) = Cont (f . g)

-- so what can we do with functor? as usual fmap it...

foobar1 = (\x -> x * 10) <$> fa
  where
    fa = Cont (\i -> i + 1)

-- runCont foobar1 3 === 40

-- ok, than we need to define applicative, since monad is
-- 'subtype' of applicative

instance Applicative (Cont a) where
  pure x = Cont (\_ -> x)
  (Cont f) <*> (Cont g) = Cont (\x -> (f x) (g x) )

-- by the way, nothing 'deep' or 'mystical' here.
-- just know I'll need applicative (it's useful by
-- it's own right sometimes, and trying to follow
-- the types. Didn't check the laws, though. Had
-- to write down the types, ... should internalize
-- this somehow...)
-- good thing about monad is that incapsulates it's
-- effect, so you know already that the only specific
-- method should somehow embedd this knowledge

instance Monad (Cont a) where
  (Cont g) >>= f  = Cont $ \a0 ->
                             let r0 = g a0
                                 (Cont h) = f r0
                             in h a0


-- btw, forgot why we need applicative. To combine
-- multiple 'wrapped' values in a single function.
-- Or to lift a multi-param function to Cont, if you prefere

-- all right, promissed applicative example with
-- belts and whistles

f0 = Cont (*2)
f1 = Cont (*3)


f3 = (liftA2 (+)) f0 f1

f3' = (+) <$> f0 <*> f1

-- I hope could be extended further:

plus3 a b c = a + b + c

f4' = plus3 <$> f0 <*> f0 <*> f1

-- runCont f3' 1 === 5

--- all right, proving monad laws could be difficult,
-- but why not giving it a try

-- left identity:
--  return a >>= k  =  k a

leftIdentity = res ((pure 3 >>= f)) == res(f 3)
    where
      f = \x -> Cont (\a -> a + x)
      res x = runCont x 3

--  m >>= return  =  m
rightIdentity = res ((m >>= pure)) == res(m)
    where
      m = Cont (\a -> a + 3)
      res x = runCont x 3

-- associativity: (everything is clearer with kleisli arrows) : FIXME: prove using Kleisli, normal tests
-- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
assoc = res (m >>= (\x -> k x >>= h)) == res((m >>= k) >>= h)
    where
      m = Cont (\a -> a + 3)
      k = \x -> Cont (\a -> a * x)
      h =  \x -> Cont (\a -> a - x)
      res x = runCont x 4

-- looks like working ... how do we use such a beast

method :: Cont Integer Integer
method = do
           x <- Cont (* 3)
           y <- Cont (+ 2)
           return $ x - y

-- ok, so monad is simple and neat. It is promised that we'll be
-- able to do some control structures using cont ...

-- references:
-- https://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Applicative.html

-- https://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Monad.html
