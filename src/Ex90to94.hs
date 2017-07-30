module Ex90to94 (

) where

import Data.List(group, sort, findIndex, intersect, unfoldr, intersperse, nubBy, nub,
                    (\\), delete, inits, tails, sortBy, sortOn, permutations)
import Data.Maybe(fromJust, isJust)
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Monad.Loops(iterateWhile, unfoldM)
import Control.Applicative(liftA, Alternative, many)
import Control.Monad(ap, MonadPlus, mplus)
import qualified Control.Applicative as App
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-------- Eight queens, finaly ------------

type Pos = (Int, Int)

-- super lame)
eightqueens :: [[Pos]]
eightqueens =  do
    q1 <- putQ 1
    q2 <- filter (notThreatening [q1]) (putQ 2)
    q3 <- filter (notThreatening [q1, q2]) (putQ 3)
    q4 <- filter (notThreatening [q1, q2, q3]) (putQ 4)
    q5 <- filter (notThreatening [q1, q2, q3, q4]) (putQ 5)
    q6 <- filter (notThreatening [q1, q2, q3, q4, q5]) (putQ 6)
    q7 <- filter (notThreatening [q1, q2, q3, q4, q5, q6]) (putQ 7)
    q8 <- filter (notThreatening [q1, q2, q3, q4, q5, q6, q7]) (putQ 8)
    return [q1, q2, q3, q4, q5, q6, q7, q8]
  where
    n = 8
    putQ x = map (\y -> (x,y)) [1..n]
    sameDiagonal (x0, y0) (x1, y1) = x1 /= x0 && (k == 1 || k == (-1) ) where
      k = (fromIntegral $ y1 - y0) / (fromIntegral $ x1 - x0)
    sameRowOrCol (x0, y0) (x1, y1) = x0 == x1 || y0 == y1
    notThreatening  xs q = all (\x -> (not $ sameDiagonal q x) && (not $ sameRowOrCol q x) ) xs

nqueens :: Int -> [[Pos]]
nqueens n = foldl f [[]] [1..n]
  where
    f ma r = ma >>= (\queens ->
                      filter (notThreatening queens) (putQ r) >>= (\q ->
                        return $ queens ++ [q]
                      )
                    )

    putQ x = map (\y -> (x,y)) [1..n]
    sameDiagonal (x0, y0) (x1, y1) = x1 /= x0 && (k == 1 || k == (-1) ) where
      k = (fromIntegral $ y1 - y0) / (fromIntegral $ x1 - x0)
    sameRowOrCol (x0, y0) (x1, y1) = x0 == x1 || y0 == y1
    notThreatening  xs q = all (\x -> (not $ sameDiagonal q x) && (not $ sameRowOrCol q x) ) xs
