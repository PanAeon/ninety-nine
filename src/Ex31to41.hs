module Ex31to41
    (
       isPrime
     , gcd'
     , isCoprime
     , phi
     , primeFactors
    ) where




import Control.Monad
import Control.Applicative
import Control.Arrow((&&&))
import Data.List(tails, group, unfoldr,sortBy, sortOn, concat, sort, find, groupBy)
import Data.Maybe(listToMaybe, fromJust)
import Data.List.Zipper
import System.Random
import Ex11to20(removeAt')
import qualified Data.Map as Map
import Data.Function(on)


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
primeFactors n = undefined
