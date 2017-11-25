module WhyFPMatters() where

-- from https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf

sum' :: [a] -> Int
sum' [] = 0
sum' (x:xs) = 1 + sum' xs

sum'' = foldr' (+) 0

foldr' f z [] = z
foldr' f z (x:xs) = f x (foldr' f z xs)

append' a b = foldr' (:) b a

doubleandcons = foldr' doubleall []
   where
     doubleall n xs = (2:n):xs

next n x = (x + n/x)/2

repeat' f a = a : (repeat' f (f a))


within eps (a:b:rest)
    | abs (a - b) < eps = b
    | otherwise         = within eps (b:rest)

sqrt' a0 eps n = within eps (repeat' (next n) a0)


easydiff f x h = (f (x+h) - f x) / h

differentiate h0 f x = map (easydiff f x) (repeat' halve h0)
             where
               halve x = x / 2


-- FIXME: doit , https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf
-- esp. alphabeta
