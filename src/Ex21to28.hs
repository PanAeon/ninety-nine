module Ex21to28
    (
        insertAt'
      , range'
      , rndSelect
      , diffSelect
      , randomPermutation
      , combinations
    ) where




import           Control.Applicative
import           Control.Arrow       ((&&&))
import           Control.Monad
import           Data.Function       (on)
import           Data.List           (concat, find, group, groupBy, sort,
                                      sortBy, sortOn, tails, unfoldr)
import           Data.List.Zipper
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust, listToMaybe)
import           Ex11to20            (removeAt')
import           System.Random

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
-- TODO: performance?
combinations:: Int -> [a] -> [[a]]
combinations 0 xs = []
combinations 1 xs = map (\x -> x:[]) xs
combinations n (x:xs) = (map ((:)x) $ combinations (n - 1) xs) ++ combinations n xs


-- Group the elements of a set into disjoint subsets.
{-
example:
group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]


group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]

-}


group' :: [Int] -> [a] -> [[[a]]]
group' [] [] = []
group' (n:[]) xs = [[xs]]
group' gs xs =   foobar (\g rest ->
                           map   (\bs ->
                             (take g xs): bs
                           ) (group' rest (drop g xs))
               ) gs



foobar :: (a -> [a] -> [b]) -> [a] -> [b]
foobar f xs = concat $ foldrz (\z res -> f (cursor z) (toList $ delete z) : res) [] z0
        where
          z0 = fromList xs


-- Problem 28
-- Sorting a list of lists according to length of sublists
lsort :: [[a]] -> [[a]]
lsort = sortOn length

{-
Again, we suppose that a list contains elements that are lists themselves.
But this time the objective is to sort the elements of this list according to
their length frequency;
i.e., in the default, where sorting is done ascendingly, lists with rare lengths
are placed first, others with a more frequent length come later.
-}
-- TODO: sortBy , comparing, study
lfsort :: [[a]] -> [[a]]
lfsort xs = sortOn (\x ->  snd $ fromJust $ find (\y -> (fst y) == (length x)) freqs) xs
    where
      freqs =  map (\x -> (head x, length x)) $ group . sort $ (map length xs)



lfsort' :: [[a]] -> [[a]]
lfsort' = concat . lsort . groupBy ((==) `on` length) . lsort
