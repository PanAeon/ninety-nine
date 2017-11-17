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
import Debug.Trace
import Data.Tuple(swap)
import Data.Maybe(maybeToList)

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

-- First more or less valid result:
-- ,[(6,2),(7,3),(8,0),(8,4),(9,1),(9,5),(10,2),(11,3),(12,1),(13,0),(16,5),(17,4),(18,7),(19,6),(22,11),(23,10),(24,13),(25,12),(28,17),(29,16),(30,19),(30,26),(31,18),(31,27),(32,24),(32,28),(33,25),(33,29),(34,23),(34,26),(35,22),(35,27)])
-- second meaningful solution:
-- 6 !! (181,[(6,2),(7,3),(8,0),(8,4),(9,5),(10,2),(11,3),(12,1),(13,0),(14,1),(15,4),(15,11),(16,5),(17,9),(18,7),(19,6),(21,17),(23,10),(24,13),(25,12),(26,22),(27,14),(28,20),(29,16),(30,19),(30,26),(31,18),(31,20),(32,24),(32,28),(33,25),(33,29),(34,21),(34,23),(35,22),(35,27)])

-- 8 !! (first solution!!!)
{- [(8,2),(9,3),(10,0),(10,4),(11,1),(11,5),(12,2),(12,6),(13,3),(13,7),(14,4),(15,5),(16,1),(16,10),(17,0),(17,2),(17,11),(18,1),(18,3),(18,8),(18,12),(19,2),(19,4),(19,9),(19,13),(20,3),(20,5),(20,10),(20,14),(21,4),(21,6),(21,11),(21,15),(22,5),(22,7),(22,12),(23,6),(23,13),(24,9),(24,18),(25,8),(25,10),(25,19),(26,9),(26,11),(26,16),(26,20),(27,10),(27,12),(27,17),(27,21),(28,11),(28,13),(28,18),(28,22),(29,12),(29,14),(29,19),(29,23),(30,13),(30,15),(30,20),(31,14),(31,21),(32,17),(32,26),(33,16),(33,18),(33,27),(34,17),(34,19),(34,24),(34,28),(35,18),(35,20),(35,25),(35,29),(36,19),(36,21),(36,26),(36,30),(37,20),(37,22),(37,27),(37,31),(38,21),(38,23),(38,28),(39,22),(39,29),(40,25),(40,34),(41,24),(41,26),(41,35),(42,25),(42,27),(42,32),(42,36),(43,26),(43,28),(43,33),(43,37),(44,27),(44,29),(44,34),(44,38),(45,28),(45,30),(45,35),(45,39),(46,29),(46,31),(46,36),(47,30),(47,37),(48,33),(48,42),(49,32),(49,34),(49,43),(50,33),(50,35),(50,40),(50,44),(51,34),(51,36),(51,41),(51,45),(52,35),(52,37),(52,42),(52,46),(53,36),(53,38),(53,43),(53,47),(54,37),(54,39),(54,44),(55,38),(55,45),(56,41),(56,50),(57,40),(57,42),(57,51),(58,41),(58,43),(58,48),(58,52),(59,42),(59,44),(59,49),(59,53),(60,43),(60,45),(60,50),(60,54),(61,44),(61,46),(61,51),(61,55),(62,45),(62,47),(62,52),(63,46),(63,53)] -}
iterateKnightTour :: (Int, [(Int, Int)])
iterateKnightTour = until (\x -> fst x > 100 || isKnightTour _N (snd x)) f' (0, [])
  where
    _N = 6
    f' (step, _) = ( step + 1, map fst $ filter (\foo -> snd foo == 1) $ Map.toList $ runNetwork (drop (_N*_N*8*step) randomz) _N)


--iterateKnightTour2 :: Int
iterateKnightTour2 =  (fromIntegral $ snd $ until (\x -> fst x > _M ) f' (0, 0)) / (fromIntegral _M)
  where
    _N = 6
    _M = 100
    f' (step, foo) = ( step + 1,  foo + (length $ filter (\foo -> snd foo == 1) $ Map.toList $ runNetwork (drop (_N*_N*8*step) randomz) _N))


isKnightTour :: Int  -> [(Int, Int)] -> Bool
isKnightTour n xs = allElementsAppearExactlyTwice ys && length xs == (n*n) && reallyIsKnightTour n xs --length xs > 0-- && length xs == (n*n)
   where
     allElementsAppearExactlyTwice = all ((==2) . length) . group . sort
     ys = xs >>= \x -> [fst x, snd x]

reallyIsKnightTour :: Int -> [(Int, Int)] -> Bool
reallyIsKnightTour n xs = go start == n * n
  where
    m0 = Map.fromList xs
    m1 = Map.fromList $ fmap swap xs
    lkup i prev = head $ delete prev $ delete prev $ maybeToList (Map.lookup i m0) ++ maybeToList (Map.lookup i m1)
    start@(s0,s1) = head xs
    go curr@(p,c) = if (curr == start) then 1
              else 1 + go (c, lkup c p)


runNetwork :: Randomz -> Int -> Map (Int, Int) Int
runNetwork rz n = runNetwork' 0 limit n u0 v0
  where
    limit = 200
    u0 = initU n
    v0 = initV (drop 0 rz) n
    --getV (_, v, _) = v

runNetwork' :: Int -> Int -> Int -> Map (Int, Int) Int -> Map (Int, Int) Int -> Map (Int, Int) Int
runNetwork' s limit n _U _V
       | s > limit = trace "diverged" $ _V
       | otherwise = if go then
                       runNetwork' (s + 1) limit n _U' _V'
                     else
                       _V'
     where
       (_U', _V', go) = runStep n _U _V

runStep :: Int -> Map (Int, Int) Int -> Map (Int, Int) Int -> (Map (Int, Int) Int, Map (Int, Int) Int, Bool)
runStep n _U _V = (_U', _V', _U /= _U')
  where
    _U' = Map.mapWithKey updateU _U
--    updateU (i, j) u = u - sum (fmap (_V Map.! ) (neighbours i n))
    updateU (i,j) u = min 10 (max (-10) r0)
       where
        r0 = u + 4 - sum (fmap (_V Map.! ) ( {- delete (i,j) $ nub $ -} (neighbours i n ++ neighbours j n)))
    _V' = Map.mapWithKey updateV _V
    updateV (i,j) v
       | _U' Map.! (i,j) > 3 = 1
       | _U' Map.! (i,j) < 0 = 0
       | otherwise      = v



 --   nChanged = Map.foldlWithKey (\s n v -> s + updateV n v) 0 _V -- this is wrong


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

-- initNeurons :: Int -> Map (Int, Int) Neuron
-- initNeurons n = Map.fromList $ foo <$> zip (genMoves n) randomz
--   where
--     foo (c, r) = (c, Neuron 0 r)

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
randomz = randomRs (0, 1) g
  where
    g = unsafePerformIO getStdGen

packCoords i j n = i + j * n
unpackCoords k n = (k `mod` n, k `div` n)
