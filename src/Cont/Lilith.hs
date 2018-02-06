module Cont.Lilith where

-- from http://blog.sigfpe.com/2008/12/mother-of-all-monads.html

import Control.Monad.Cont

ex1 :: Monad m => m Int
ex1 = do
  a <- return 1
  b <- return 10
  return $ a + b

-- For the Cont monad we get something that takes a function,
--  and applies it to 11.

-- ex2 :: Monad m => m Int

-- reification of argument passing
ex2 = do
  a <- return 1
  b <- cont $ \fred -> 10 -- will ignore the results
  return $ a + b
-- not that above result is bound (by 10)


ex3 = do
  a <- return 1
  b <- cont $ \fred -> fred(10)
  return $ a + b

ex4 = do
  a <- cont $ \bob -> 1 + bob(1)
  b <- cont $ \fred -> 1 + fred(10)
  return $ a + b

ex4' = let
         fa = \bob -> 1 + bob(1)
         fb = \fred -> 1 + fred(10)
       in
         fa $ (\a -> fb $ (\b -> a + b))


---- emulating Maybe
eitherEx x = do
    a <- return x
    b <- cont $ (\c -> if a == 3 then "escape" else c(3))
    return $ a + b

-- emulating list
listEx1 = do
  a <- return 1
  b <- cont (\fred -> fred 10 ++ fred 20)
  return $ a + b

ex7 = do
  a <- return 1
  b <- cont (\fred -> concat [fred 10, fred 20])
  return $ a + b

ex8 = do
  a <- return 1
  b <- cont (\fred -> [10,20] >>= fred)
  return $ a + b

i x = cont (\fred -> x >>= fred)

ex9 = do
  a <- i [1,2]
  b <- i [10,20]
  return $ a + b
