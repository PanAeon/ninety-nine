module Manatee.MaxSubarray where

import Data.List

ex1 = [2, 3, -1, 5, 2, -1, -3, -8, 1, 2,  3, -4, -5, 16, 16, -24, 24] :: [Int]


ex2 = [-2, -3, 4, -1, -2, 1, 5, -3] :: [Int]


maxSubarray :: [Int] -> Int
maxSubarray [] = 0
maxSubarray (x:xs) = fst $ foldl f' (x, x) xs
  where
    f' (maxSoFar, maxEndingHere) x =
      let
        maxEndingHere' = max x (maxEndingHere + x)
        maxSoFar' = max maxSoFar maxEndingHere'
      in (maxSoFar', maxEndingHere')
