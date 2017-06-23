module Ex31to41
    (
       isPrime
     , gcd'
     , isCoprime
     , phi
     , primeFactors
     , primeFactorsMult
    ) where




import Control.Monad
import Control.Monad.Loops
import Control.Applicative
import Control.Arrow((&&&))
import Data.List(tails, group, unfoldr,sortBy, sortOn, concat, sort, find, groupBy)
import Data.Maybe(listToMaybe, fromJust)
import Data.List.Zipper
import System.Random
import Ex11to20(removeAt')
import qualified Data.Map as Map
import Data.Function(on)
import Ex11to20(pack)


-- TODO: see more at https://wiki.haskell.org/99_questions/Solutions/31 for efficient impl
isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (\m -> (n == m) || ((n `mod`) m /= 0)) [2..(n `div` 2 + 1)]


--  Determine the greatest common divisor of two positive integer numbers.
gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

-- Determine whether two positive integer numbers are coprime.
-- Two numbers are coprime if their greatest common divisor equals 1.
isCoprime :: Int -> Int -> Bool
isCoprime a b = gcd' a b == 1

-- Calculate Euler's totient function phi(m).
-- number of positive integers r (1 <= r < m) that are coprime to m.
phi :: Int -> Int
phi n = length $ filter (isCoprime n) [1..n-1]


-- Determine the prime factors of a given positive integer.
-- Construct a flat list containing the prime factors in ascending order.
primeFactors :: Int -> [Int]
primeFactors n = concat $ unfoldr (uncurry f) (n, primes)
  where
    f 0 _ = Nothing
    f 1 _ = Nothing
    f n ps@(p:pps) = if (n `mod` p == 0) then Just ([p],(n `div` p, ps)) else Just([], (n,pps))
    primes = filter isPrime [2..n]

-- construct a list of prime factors and their multiplicity
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = (map swap) . encode' . primeFactors
                where
                  swap (x,y) = (y,x)

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map enc . pack
                 where
                   enc :: [a] ->  (Int, a)
                   enc [x] = (1,x)
                   enc xs    =  ($ xs) $ (,) <$> length <*> head

-- Calculate Euler's totient function phi(m) (improved).
{-
: Let ((p1 m1) (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities)
 of a given number m. Then phi(m) can be calculated with the following formula:
 phi(m) = (p1 - 1) * p1 ** (m1 - 1) *
         (p2 - 1) * p2 ** (m2 - 1) *
         (p3 - 1) * p3 ** (m3 - 1) * ...
-}
phi' :: Int -> Int
phi' n = foldr f 1 $ primeFactorsMult n
        where
          f (p, m) n = ( (p - 1) * p ^ (m - 1)) * n


-- Given a range of integers by its lower and upper limit,
-- construct a list of all prime numbers in that range.
-- naive, TODO: make it efficient?
primesR:: Int -> Int -> [Int]
primesR l h = filter isPrime [l..h]

--  Goldbach's conjecture.
goldbach :: Int -> (Int, Int)
goldbach n = (,) <$> id <*> (n-) $ fromJust $ find (isPrime . (n-)) primes
         where
           primes = filter isPrime [2..n]

{-
Given a range of integers by its lower and upper limit,
print a list of all even numbers and their Goldbach composition.

In most cases, if an even number is written as the sum of two prime numbers,
one of them is very small. Very rarely, the primes are both bigger than say 50.
Try to find out how many such cases there are in the range 2..3000.
-}
-- TODO: efficient version
goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList l r = map goldbach $ filter even [l..r]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' l r m = filter p $ goldbachList l r
                    where
                      p (a,b) = a >= m && b >= m
