module Circular.Circular() where

-- Nice post, lots of links
-- http://izzycecil.com/posts/2015-07-29-circular.html

--replace every element of a list with the minimum element of that list.


trace :: (a -> c -> (b, c)) -> a -> b
trace f a = b
  where (b, c) = f a c

-- good, looks like got it, not too bad

rplc :: Ord a => [a] -> [a]
rplc [] = []
rplc ss = res
  where
    (res, m'') = rplc' ss m''
    rplc' [x] m     = ([m], x)
    rplc' (x:xs) m = (m:ys, min x m')
      where
        (ys, m') = rplc' xs m

repminList' :: (Ord a) => [a] -> a -> ([a], a)
repminList' [x] m = ([m], x) -- here we provide initial value
repminList' (x:xs) m = let (replaced, m') = repminList' xs m
                       in (m : replaced, min x m')

-- what we do here -- we pass down m (pointer to result), and result itself

repminList :: (Ord a) => [a] -> [a]
repminList = trace repminList'

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
