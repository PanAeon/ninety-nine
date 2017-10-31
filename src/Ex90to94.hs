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
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import System.Random
import System.IO.Unsafe

-------- Eight queens, finaly ------------
-- FIXME: write bloody your own version
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

data Neuron = Neuron Int Int deriving (Eq, Show)

knightsTour :: Int -> [(Int, Int)]
knightsTour = undefined
  where
    x = 3


-- FIXME: iterate !! std until
-- FIXME: use normal random monad

isKnightTour :: Int  -> [(Int, Int)] -> Bool
isKnightTour n xs = length xs == (n*n) && allElementsAppearExactlyTwice
   where
     allElementsAppearExactlyTwice = undefined
     ys = xs >>= \x -> [fst x, snd x]


runNetwork :: Randomz -> Int -> Map (Int, Int) Int
runNetwork rz n = runNetwork' 0 limit n u0 v0
  where
    limit = 200
    u0 = initU n
    v0 = initV rz n
    --getV (_, v, _) = v

runNetwork' :: Int -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int -> Map (Int, Int) Int
runNetwork' s limit n _U _V
       | s > limit = error "doesn't converge"
       | otherwise = if go then
                       runNetwork' (s + 1) limit n _U' _V'
                     else
                       _V
     where
       (_U', _V', go) = runStep n _U _V

runStep :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int -> (Map (Int, Int) Int, Map (Int, Int) Int, Bool)
runStep n _U _V = (_U', _V', nChanged > 0)
  where
    _U' = Map.mapWithKey updateU _U
    updateU (i,j) u = u + 2 - sum (fmap (_V Map.! ) ((neighbours i n) ++ (neighbours j n)))
    _V' = Map.mapWithKey updateV _V
    updateV n@(i,j) v
       | _U Map.! n > 3 = 1
       | _U Map.! n < 0 = 0
       | otherwise      = v
    nChanged = Map.foldlWithKey (\s n v -> s + updateV n v) 0 _V


neighbours :: Int -> Int -> [(Int, Int)]
neighbours i n = normalize <$> validMoves i n
  where
    normalize j =  if i > j then
                      (i, j)
                    else
                      (j, i)

type Randomz = [Int]
-- neighbours' :: Int -> Int -> Int -> [(Int, Int)]
-- neighbours' i j n = undefined

-- state function
initU :: Int -> Map (Int, Int) Int
initU n = Map.fromList $ zip (genMoves n) (repeat 0)

-- outputs
initV :: Randomz -> Int -> Map (Int, Int) Int
initV rz n = Map.fromList $ zip (genMoves n) rz

initNeurons :: Int -> Map (Int, Int) Neuron
initNeurons n = Map.fromList $ foo <$> zip (genMoves n) randomz
  where
    foo (c, r) = (c, Neuron 0 r)

genMoves :: Int -> [(Int, Int)]
genMoves n = [ (i, j) |
               i <- [0..n*n-1],
               j <- validMoves i n,
               i > j]

validMoves k n = [packCoords x y n | dx <- [-1,-2,1,2],
                                     dy <- [-1,-2,1,2],
                                     let x = x0 + dx,
                                     let y = y0 + dy,
                                     abs dx /= abs dy,
                                     x >= 0 && x < n,
                                     y >= 0 && y < n]
  where
    (x0, y0) = unpackCoords k n

toKey :: Int -> Int -> Int -> Int
toKey a b n = a + b * (n * n)

{-# NOINLINE randomz #-}
randomz :: [Int]
randomz = drop 58 $ randomRs (0, 1) g
  where
    g = unsafePerformIO getStdGen

packCoords i j n = i + j * n
unpackCoords k n = (k `mod` n, k `div` n)
