module Ex21to28
    (
        insertAt'
      , range'
      , rndSelect
      , diffSelect
      , randomPermutation
      , combinations
    ) where




import Control.Monad
import Control.Applicative
import Control.Arrow((&&&))
import Data.List(tails, group, unfoldr,sortBy, sortOn)
import Data.Maybe(listToMaybe)
import Data.List.Zipper
import System.Random
import Ex11to20(removeAt')

-- zippers ! yay
insertAt':: Int -> a -> [a] -> [a]
insertAt' k x xs = toList . insert x  $ foldr ($) (fromList xs) (replicate (k-1) right)

-- Create a list containing all integers within a given range.

range :: Int -> Int -> [Int]
range k n = drop (k-1) . take n $ [1..]

range' :: Int -> Int -> [Int]
range' k n | k <= n    = k : range' (k+1) n
           | otherwise = []

range'' x y = [x..y]

-- Extract a given number of randomly selected elements from a list.
-- TODO: how to test random select, huh?
rndSelect :: Int -> [a] -> IO [a]
rndSelect n xs = do
  gen <- newStdGen
  let rs = randomRs (0, length xs - 1) gen :: [Int]
  return $ map (xs!!) (take n rs) -- unfold??

-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
diffSelect:: Int -> Int -> IO [Int]
diffSelect n m = do
                  gen <- newStdGen
                  return $ take n $ unfoldr generate (gen, [1..m])
            where
              generate (g,xs) = let (n, g') = randomR (0, length xs - 1) g
                                    (x, xs') = (removeAt' n xs)
                                in
                                  Just (x, (g', xs'))

randomPermutation:: [a] -> IO [a]
randomPermutation xs = newStdGen >>= \gen ->
            return $ (map snd) . (sortOn fst) $ zip (randoms gen :: [Int]) xs


-- Generate the combinations of K distinct objects chosen from the N elements of a list

combinations:: Int -> [a] -> [[a]]
combinations = undefined
