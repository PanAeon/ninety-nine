module Dynamic() where

import Data.Array
import Data.List(unfoldr)
-- memoize?
slow_ways :: (Int, Int) -> Integer
slow_ways (0,_) = 1
slow_ways (_,0) = 1
slow_ways (i,j) = slow_ways (i-1,j) + slow_ways (i, j-1)

-- https://wiki.haskell.org/Memoization

-- my memoized is slower :)
-- with arrays even slower..
-- single list a little better then array :)
-- pass mutable state inside??
memoized_ways :: (Int, Int) -> (Int, Int) -> Integer
memoized_ways (n,m) (r,c) =
     [ ways (i,j) | i <- [0..n-1], j <- [0..m-1]] !! (r + n * c)
  where
    ways (0,_) = 1
    ways (_,0) = 1
    ways (i,j) = memoized_ways (n,m) (i-1,j) + memoized_ways (n,m) (i, j-1)

-- compute first row, and map that row..
-- you need ,current row, and prev row
-- wow, this is bloody fast!
m_ways :: (Int, Int) -> Integer
m_ways (n,m) = resultRow !! (m)
  where
    firstRow = [1 | i <- [0..m]]
    resultRow = foldl f firstRow [0..n-1]
    f prev _ = tail $ scanl (+) (0) prev


slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
