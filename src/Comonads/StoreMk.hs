{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}


module Comonads.StoreMk () where

import Control.Comonad
import Control.Applicative
import Data.Functor.Const
import Data.Functor.Identity
-- replace with Applicative and you got the Bazaar
newtype Pretext s a = Pretext {
    runPretext :: forall f. Functor f => (s -> f s) -> f a
  } deriving Functor

experiment :: Functor f => (s -> f s) -> Pretext s a -> f a
experiment f (Pretext k) = k f

--  exercise  -- create comonad instance,
-- "Defining the Comonad instance for that type is a particularly
-- enlightening challenge." Edward Kmett


instance Comonad (Pretext s) where
   extract (Pretext f) = runIdentity $ f Identity
   duplicate pa = Pretext (\k1 -> (\s -> Pretext (\k2 -> l' <$> k2 s)) <$> k1 s0)
     where
       s0 = getConst $ runPretext pa (Const)
       l' x = runIdentity $ runPretext pa (Identity . const x)





-- extract . duplicate      = id                         ✓ ✗ ✓
-- fmap extract . duplicate = id                         ✗ ✓ ✓
-- duplicate . duplicate    = fmap duplicate . duplicate ✓ ✗ ✓
-- (last one was hard to check,
-- FIXME: guyz, how do you check this props, on paper?)

p0 :: Pretext String Int
p0 = Pretext (\f -> length <$> f "hello")

p1 :: Pretext String (Pretext String Int)
p1 = Pretext (\k1 -> (\s -> Pretext (\k2 -> l' <$> k2 s)) <$> k1 s0)
  where
    s0 = getConst $ runPretext p0 (Const)
    l' x = runIdentity $ runPretext p0 (Identity . const x)

p2 :: Pretext String (Pretext String Int)
p2 = Pretext (\k1 -> (\s -> Pretext (\k2 -> length <$> k2 s)) <$> k1 "hello") -- so you need to pull length from p0

foo :: Pretext String Int -> Pretext String Int
foo = extract . duplicate

t1 = runPretext (foo p0) (Just . (++" world"))


newtype Bazaar s a = Bazaar {
    runBazaar :: forall f. Applicative f => (s -> f s) -> f a
  } deriving Functor

experiment' :: Applicative f => (s -> f s) -> Bazaar s a -> f a
experiment' f (Bazaar k) = k f

b0 :: Bazaar String Int
b0 = Bazaar (\f -> length <$> f "hello")

b2 :: Bazaar String (Bazaar String Int)
b2 = Bazaar (\k1 -> (\s -> Bazaar (\k2 -> length <$> k2 s)) <$> k1 "hello") -- so you need to pull length from p0


-- FIXME: write comonad for bazaar
-- instance Comonad (Bazaar s) where
--    extract (Bazaar f) = runIdentity $ f Identity
--    duplicate ba =

data FunList s a
    = Done a
    | More s (FunList s (s -> a))
    deriving Functor
-- (<*>) :: f (a -> b) -> f a -> f b
experiment'' :: Applicative f => (s -> f s) -> FunList s a -> f a
experiment'' k (Done a) = pure a
experiment'' k (More s fl) = (experiment'' k fl) <*> k s

-- FIXME: exercise - define Applicative and Comonad

instance Applicative (FunList s) where
  pure = Done
  Done ab <*> fa  = ab <$> fa
  More s fl <*> fa = More s (flip <$> fl <*> fa) -- FunList s (s -> b)


 
-- Failed attempts for Pretext:

 -- (Pretext s a -> b) -> Pretext s a -> Pretext s b
 -- extend f pa = (\a -> fmap (const a) pa ) <$> pa
 -- extend f pa = Pretext (\l -> -- s -> f s
 --                 let
 --                   x = runPretext pa l -- f a, we apply l (outer)
 --                   p0 = Pretext (\_ -> x)
 --                   z = f pa            -- we apply inner, now we have two values
 --                   --p0 = Pretext ()
 --                 in fmap (\a -> (f pa)) x -- a is lost
 --               )
 -- duplicate pa = Pretext (\k ->
 --            let
 --              a = runIdentity $ runPretext pa (Identity)
 --              s = getConst $ runPretext pa (Const)
 --              foo k' a = Pretext (\s1 -> runPretext pa s1)
 --              p0 = Pretext (\k' -> (foo k') <$> k' s)
 --              z = runPretext p0 k -- fa
 --            in z
 --          )

 -- dup :: Pretext s a -> Pretext s (Pretext s a)
 -- dup pa = Pretext (\k ->
 --            let
 --              a = runIdentity $ runPretext pa (Identity)
 --              s = getConst $ runPretext pa (Const)
 --              foo k' a = Pretext (\s1 -> runPretext pa s1)
 --              p0 = Pretext (\k' -> (foo k') <$> k' s)
 --              z = runPretext p0 k -- fa
 --            in z
 --          )


   -- (\a ->
                  -- Pretext (\k1 -> (\s' -> a)<$> k1 s)
                -- ) <$> pa
            -- where
              -- s =  getConst $ runPretext pa (Const)

   --(\a -> fmap (const a) pa ) <$> pa

   --Pretext (\g ->
                --  fmap (\a -> Pretext(\h ->
                --     (runPretext pa h )
                --  )) (runPretext pa g )
                --)
-- (s -> f s) -> f a ==> (s -> f s) -> f (Prextext a)



  -- Pretext (\g ->
  --           (\s' -> Pretext (\s'' -> fmap (\s''' -> a)(s'' s'))) <$> (g s)
  --        )
  --  where
  --    s = getConst $ runPretext pa (Const)
  --    a = runIdentity $ runPretext pa (Identity)
