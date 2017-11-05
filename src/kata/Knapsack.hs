module Knapsack () where



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
