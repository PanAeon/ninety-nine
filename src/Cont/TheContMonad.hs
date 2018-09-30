module Cont.TheContMonad
    (
    ) where

-- from http://www.haskellforall.com/2012/12/the-continuation-monad.html
import Control.Monad

newtype Cont r a = Cont { runCont :: (a -> r) -> r }
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }


onInput :: (String -> IO ()) -> IO ()
onInput f = forever $ do
    str <- getLine
    f str

-- SumType = A|B|C
-- ContT <return> <Monad> <SumType>
-- which allows to group multiple callback into single signature

{-
thoughts∷
I firmly believe that the way to a Monads heart is through its Kleisli arrows,
and if you want to study a Monads "purpose" or "motivation" you study what
its Kleisli arrows do∷

  a -> Cont r b
~ a -> (b -> r) -> r    -- Expand the definition of Cont
~ (b -> r) -> (a -> r)  -- Flip the arguments

Earlier I said that the key to a monad is its Kleisli arrows.
The reason why is that Kleisli arrows are morphisms in the Kleisli category,
 where (>=>) is Kleisli arrow composition:
 and return is the identity

>=> :: (a -> m b) -> (b -> m c) -> (a -> m c)
return :: a -> m a
-}
