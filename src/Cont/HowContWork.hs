module Cont.HowContWork where

-- From https://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work


-- it represents the rest of computation


{-
Questions:

1. Why direct approach will not work?? (\a -> foo a z)
2. Why flip ($) is continuation?

-}


-- Recall

bar = (+)

foo = (+)

example1 x y z = foo (bar x y) z

rest' :: Int -> (Int -> Int)
rest' z = \a -> foo a z

-- extract the computation we are interested in
-- and apply to a function that is the rest of
-- computation

rest :: Int -> Int -> (Int -> t) -> t
rest x y = \f -> f (bar x y)

infix 3 |>
(|>) =  \x k -> k x  --  ( it's just a function application)


z x y =  (|>) (bar x y)

-- :t flip ($) True
