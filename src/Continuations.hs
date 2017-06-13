module Continuations(calculateLength) where

import Control.Monad.Cont
import Control.Monad.Trans.Cont(shift, reset)


calculateLength :: [a] -> Cont r Int
calculateLength l = return (length l)

double :: Int -> Cont r Int
double n = return (n * 2)


whatsYourName :: String -> String
whatsYourName name = (`runCont` id) $
    callCC $ \exit ->
      validateName name exit >>= \_ ->
      return $ "Welcome, " ++ name ++ "!"

validateName:: String -> (String -> Cont r ()) -> Cont r ()
validateName name exit =
  if (null name) then (exit "You forgot to tell me your name!") else return ()

ex2 = do
  a <- return 1
  b <- cont (\fred -> 10)
  return $ a + b

-- runCont ex4 (\x -> [x])
ex4 :: Cont [a] Int
ex4 = do
  a <- return 1
  b <- cont (\fred -> fred 10 ++ fred 20)
  return $ a + b


innerest :: Int -> Cont String Int
innerest x = cont $ \f -> "something: " ++ (f x)

inner :: (a -> Cont r b) -> Cont r a
inner f = cont $ \x -> undefined

callCC' :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC' f = cont $ \h ->
             runCont (f (\a -> cont $ \_ -> h a)) h

-- callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) $ notOk "Denominator 0"
    ok $ x `div` y
  handler err



divExcpt' :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt' x y handler = callCC $ \ok ->
    (callCC $ \notOk ->
      (when (y == 0) $ notOk "Denominator 0") >>= \_ -> ok $ x `div` y) >>= \err ->
         handler err

divExcpt'' ::  Int -> Int  -> Cont r Int
divExcpt'' x y  = callCC $ \ok ->
     if y /= 0 then (ok $ x `div` y) else (ok 0)--(cont $ \x -> 0)

divExcpt''' :: Int -> Int  -> Cont r Int
divExcpt''' x y  = callCC' $ \ok ->
       double 4 >>= \d ->
          (ok $ (x `div` y + d)) >>= \z ->
            return $ z + 1

-- Cont monad under the hood

f :: Int -> Cont r Int
f x = cont $ \c -> c $ x * 3

g :: Int -> Cont r Int
g x = cont $ \c -> c $ x - 2

f' x = return (x * 3)
g' x = return (x - 2)

h :: Int -> Cont r Int
h x | x == 5    = f x
    | otherwise = g x

doC :: Cont r Int
doC = return 5 >>= h

finalC :: Show a => a -> String
finalC x = "Done: " ++ show x


h' :: Int -> (Int -> Cont r Int) -> Cont r (Int)
h' x abort | x == 5 = f x
          | otherwise = abort (-1)


-- doC' n = return n >>= \x ->
--           callCC (\abort -> h' x abort) >>= \y ->
--           g y
doC' x = (callCC $ \abort ->
            if (x == 5) then f x
            else abort (-1) >>= g
         ) >>= g >>= g

doC'' x = (callCC $ \abort ->
             validateX x abort >>= f
          )

doC''' :: Int -> Cont r Int
doC''' x = (callCC $ \abort ->
             (f x) >>= \z ->
               (if (x /= 5) then (abort (-1)) else return z) >>= \y ->
                 return $ y + 100
          ) >>= \x -> return $ x + 1

validateX:: Int -> (Int -> Cont r Int) -> Cont r Int
validateX x abort =
  if (x /= 5) then (abort (-1)) else return x

t1 = liftM2 (-)
  (reset
  (liftM2 (+) (return 3)
    (shift (\k -> liftM2 (*) (return 5) (return 2)))))
  (return 1)

t2 = liftM2 (-)
    (reset
      (liftM2 (+) (return 3)
        (shift (\k -> return $ 5 * 2))))
    (return 1)
{-
validateName name exit >>= \_ ->
return $ "Welcome, " ++ name ++ "!"
-}
bind' :: (a -> Cont r b) -> Cont r a -> Cont r b
bind' f ma = cont $ \c ->
              runCont ma $ \x ->
                runCont (f x) c

bind'' :: (a -> Cont r b) -> Cont r a -> Cont r b
bind'' f ma = let h c = runCont ma c
                  g c = \a -> runCont (f a) c
             in cont $ \c -> h (g c)






foo :: a -> b -> b
foo = undefined
