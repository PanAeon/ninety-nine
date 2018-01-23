{-# LANGUAGE ExplicitForAll #-}

module Manatee.UnderstandingYoneda
    (
    ) where

-- from: https://bartoszmilewski.com/2013/05/15/understanding-yoneda/



imager :: forall r . ((Bool -> r) -> [r])
imager iffie = iffie <$> [True, False, True, False]

data Color = Red | Green | Blue deriving Show
data Note  = C | D | E | F | G | A | B deriving Show

colorMap x = if x then Blue else Red
heatMap  x = if x then 32 else 212
soundMap x = if x then C else G

idBool :: Bool -> Bool
idBool x = x

f :: Color -> String
f = show 

-- representable categories - 'represented' or mapped into set

{-
One of the things Yoneda showed is that there is at least one canonical functor from any so called locally small category
into the category of sets and functions.
-}

-- A category is locally small if the collection of morphisms between any two objects forms a set.

{- Ha! That's all to it for haskell
   FIXME: Yoneda Lema !! reread the post again (first time understand something, but
          the state of conciousness has failed me)
   forall r . ((a -> r) -> f r) ~ f a

   (wait, not so easy)
-}
--
-- data List a r = List { runList :: forall f . Functor f => (a -> r) -> f r
--                  }
