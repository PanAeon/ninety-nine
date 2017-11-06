module Knapsack () where

import Data.Map(Map)
import qualified Data.Map as Map
import Debug.Trace

-- simple (brute-force) version
knapsack :: [Int] -> [Int] -> Int -> Int
knapsack [] _  _ = 0
knapsack _  _  0 = 0
knapsack (w:ws) (v:vs) capacity =
  if w > capacity then
    knapsack ws vs capacity
  else
    max
       (v + knapsack ws vs (capacity - w))
       (knapsack ws vs capacity)

knapsack' :: [Int] -> [Int] -> Int -> Int
knapsack' ws0 vs0 _W0 = rs Map.! (length vs0-1, _W0)
  where
    m0 = Map.fromList $ [((0, j), 0) | j <- [0.._W0]]
    zs = [(i, w, v, w') | w' <- [1.._W0],
                       ((w,v),i) <- (zip (zip ws0 vs0) ([0..]))]
    rs = foldl knapsackStep m0 zs


-- m [0, w] = 0
-- m [i, w] = m [i- 1, w] if wi > w
-- m [i, w] = max(m [i-1, w], m[i-1, w-wi] + vi) if wi <= w


type Cache = Map (Int, Int) Int

-- no, continuation, fuck
knapsackStep :: Cache -> (Int, Int, Int, Int) -> Cache
knapsackStep m p@(i, w, v, _W) =  if i == 0 || _W == 0 then
                                   Map.insert (i,_W) 0 m

                                else if w > _W then
                                   Map.insert (i,_W) (m Map.! (i - 1, _W)) m
                                else
                                  Map.insert (i,_W) (max
                                      (m Map.! (i - 1, _W))
                                      (v + m Map.! (i - 1, _W - w))
                                      ) m
