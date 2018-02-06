module Cont.MyCont
    (
    ) where


import Control.Applicative(liftA2)

-- FIXME: try to answer the questions (and of course write them)

{-
  Q: Why the signature is (a -> r) -> r ?
  A: consider y callback = let r = doWork in  (callback r)
     it's signature is (a->r) -> r
  Q: Could it be
-}

-- all right, so let's have the signature as above^
{-
   \a -> foo a z; let's do a little inversion of control:
   here we do do
   \k -> k (bar x y)

-}

bar = (+)

foo = (*)
foo' z = \a -> foo a z -- in this case a is a parameter of foo

-- how to show that this is not that convenient? FIXME: write some expressions with it

data User = User { name :: String }

-- direct encoding:
saveToDB :: User -> ()
saveToDB u = ()

getFromDB :: Int -> User
getFromDB id = User (show id)

writeLog :: User -> String -> ()
writeLog u msg = ()

sendTo :: User -> String -> ()
sendTo u address = ()

-- so get from db and send over the wire and write log "OK"

--sendTo' :: User -> ()
sendTo' :: User -> (String -> ())
sendTo' address = \u -> sendTo address u

-- so here rest of computation encoded by just a function ??
-- let future = past $ function

-- so in this form past is completely independent of the future
------------------------------------------------------------
-- lets apply inversion of control:



-------------------------------------------------------------

-- callback is similar to \k -> k (f)

-- and we apply it with:
r1 = foo' 3 $ (bar 1 2)

bar' = \k -> k (bar 1 2) -- in this case k is rest of

r1' = bar' $ (foo 3 )

infix 3 |>
(|>) =  \x k -> k x

bar'' = (|>) (bar 1 2)
r1'' = (|>) (bar 1 2) (foo 3)


{-
Idea with (>>-) comes from
  http://www.haskellforall.com/2014/04/how-continuation-monad-works.html

-}
newtype Cont r a = Cont {
     (>>-) :: (a -> r) -> r
}

-- FIXME: bind variables with Cont, not with monads!!
instance Functor (Cont r) where
  f `fmap` ma = Cont $ \br ->  ma >>- (\a -> br (f a))

instance Applicative (Cont r) where
  pure x = Cont $ \f -> f x
  f <*> ma = Cont $ \br ->  ma >>- (\a ->
                                     f >>- (\a2b -> br (a2b a))
                                  )

instance Monad (Cont r) where
  ma >>= f = Cont $ \ _return ->  ma >>- (\a ->
                                          (f a) >>- (\b ->
                                            _return b
                                          )
                                       )
-- 'same' as ma >>- f, but returns m[b], not r
ma `bind` f = Cont $ \ _return ->  ma >>- (\a ->
                                        (f a) >>- (\b ->
                                          _return b
                                        )
                                     )
-- all right, so what's about those pesky cont monad?

-- I'm a bit fed up reading blogs. let's try to write my own mini blog.

-- Part I Simple Cont Monad
-- Points to consider:
-- * cont is a 'reification' of callbacks
-- * what is the *effect* of the cont monad?

-- let's write a simple callback method:
-- methodWithCallBack:: (a -> b) -> a -> b
-- methodWithCallBack callback a = undefined
--
-- methodReturnsCallback :: (a -> b)
-- methodReturnsCallback  = undefined

-- let's define a cont datatype (for simplicity I'll stick with a->b type)
-- don't remember why but traditionally it is smth like (() -> r),
-- without first param, but I'm doing it like a peasant
-- note that it is covariant (looks that to me)



-- data Cont a r = Cont (a->r)

-- >>- :: (Cont a r) -> a -> r
-- >>- (Cont g) a = g(a)
--
-- data Cont' a r = Cont' {
--   >>-' :: a -> r
-- }
--
-- -- let's define functor for it
-- instance Functor (Cont a) where
--   f `fmap` (Cont g) = Cont (f . g)
--
-- -- so what can we do with functor? as usual fmap it...
--
-- foobar1 = (\x -> x * 10) <$> fa
--   where
--     fa = Cont (\i -> i + 1)

-- >>- foobar1 3 === 40

-- ok, than we need to define applicative, since monad is
-- 'subtype' of applicative

-- instance Applicative (Cont a) where
--   pure x = Cont (\_ -> x)
--   (Cont f) <*> (Cont g) = Cont (\x -> (f x) (g x) )

-- by the way, nothing 'deep' or 'mystical' here.
-- just know I'll need applicative (it's useful by
-- it's own right sometimes, and trying to follow
-- the types. Didn't check the laws, though. Had
-- to write down the types, ... should internalize
-- this somehow...)
-- good thing about monad is that incapsulates it's
-- effect, so you know already that the only specific
-- method should somehow embedd this knowledge

-- instance Monad (Cont a) where
--   (Cont g) >>= f  = Cont $ \a0 ->
--                              let r0 = g a0
--                                  (Cont h) = f r0
--                              in h a0


-- btw, forgot why we need applicative. To combine
-- multiple 'wrapped' values in a single function.
-- Or to lift a multi-param function to Cont, if you prefere

-- all right, promissed applicative example with
-- belts and whistles
--
-- f0 = Cont (*2)
-- f1 = Cont (*3)
--
--
-- f3 = (liftA2 (+)) f0 f1
--
-- f3' = (+) <$> f0 <*> f1
--
-- -- I hope could be extended further:
--
-- plus3 a b c = a + b + c
--
-- f4' = plus3 <$> f0 <*> f0 <*> f1

-- >>- f3' 1 === 5

--- all right, proving monad laws could be difficult,
-- but why not giving it a try

-- left identity:
--  return a >>= k  =  k a

-- leftIdentity = res ((pure 3 >>= f)) == res(f 3)
--     where
--       f = \x -> Cont (\a -> a + x)
--       res x = >>- x 3
--
-- --  m >>= return  =  m
-- rightIdentity = res ((m >>= pure)) == res(m)
--     where
--       m = Cont (\a -> a + 3)
--       res x = >>- x 3
--
-- -- associativity: (everything is clearer with kleisli arrows) : FIXME: prove using Kleisli, normal tests
-- -- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
-- assoc = res (m >>= (\x -> k x >>= h)) == res((m >>= k) >>= h)
--     where
--       m = Cont (\a -> a + 3)
--       k = \x -> Cont (\a -> a * x)
--       h =  \x -> Cont (\a -> a - x)
--       res x = >>- x 4
--
-- -- looks like working ... how do we use such a beast
--
-- method :: Cont Integer Integer
-- method = do
--            x <- Cont (* 3)
--            y <- Cont (+ 2)
--            return $ x - y

-- ok, so monad is simple and neat. It is promised that we'll be
-- able to do some control structures using cont ...

-- references:
-- https://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Applicative.html

-- https://hackage.haskell.org/package/base-4.10.1.0/docs/Control-Monad.html
