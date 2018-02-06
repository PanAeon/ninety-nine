module Manatee.ReverseState
    (
    ) where

-- see https://lukepalmer.wordpress.com/2008/08/10/mindfuck-the-reverse-state-monad/
-- for reference


newtype RState s a = RState { runState :: s -> (a,s) }

-- instance Monoid a => Monoid (RState s a) where
  -- mempty = RState $ \s -> (s, mempty)
instance Functor (RState s) where
  f `fmap` RState r0 = RState $ \s0 -> let
                                        (a1,s1) = r0 s0
                                       in (f a1, s1)

instance Applicative (RState s) where
 pure a = RState $ \s -> (a,s)
 RState f <*> RState r0 = RState $ \s0 -> let
                                    (a1, s1) =  r0 s0
                                    (f1, s2) =  f s1

                                   in (f1 a1, s2)

instance Monad (RState s) where
 return a = RState $ \s -> (a,s)
 ma >>= f = RState $ \s -> let
                                    (a, s'') =  runState ma s'
                                    (b, s') =  runState (f a) s
                                   in (b, s'')

get = RState $ \s -> (s,s)
modify f = RState $ \s -> ((),f s)
put = modify . const

-- DONE: revisit when sober
-- OK. more or less sober
----------------------------------------------------------
-- cumulativeSums [1,2,3,4,5] = [0,1,3,6,10,15]
cumulativeSums = scanl (+) 0

evalRState s0 st = snd $ runState  st s0

computeFibs =  evalRState [] $ do
  fibs <- get -- what we want

  modify cumulativeSums

  put (1:fibs)

  return fibs

  -- hm, so, it's like

fibs' = cumulativeSums (1: fibs')

-- TODO: more interesting example
