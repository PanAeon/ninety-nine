module Dynamic() where

-- memoize?
slow_ways :: (Int, Int) -> Integer
slow_ways (1,_) = 1
slow_ways (_,1) = 1
slow_ways (i,j) = slow_ways (i-1,j) + slow_ways (i, j-1)

-- https://wiki.haskell.org/Memoization

-- my memoized is slower :)
memoized_ways :: (Int, Int) -> Integer
memoized_ways (r,c) = [ [ ways (i,j) | j <- [1..]] | i <- [1..] ] !! (r - 1) !! (c - 1)
  where
    ways (1,_) = 1
    ways (_,1) = 1
    ways (i,j) = memoized_ways (i-1,j) + memoized_ways (i, j-1)

slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
