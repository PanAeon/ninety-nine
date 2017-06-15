module Ex21to28
    (
        insertAt'
      , range'
      , rndSelect
    ) where




import Control.Monad
import Control.Applicative
import Control.Arrow((&&&))
import Data.List(tails, group)
import Data.Maybe(listToMaybe)
import Data.List.Zipper
import System.Random


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
