module Ex31to41
    (
       isPrime
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
