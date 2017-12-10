module Dynamic() where

import Data.Vector(Vector)
import qualified Data.Vector as V


-- see: https://www.cakesolutions.net/teamblogs/solving-dynamic-programming-problems-using-functional-programming-part-4

-- top down version:

get :: Int -> Int -> Vector (Vector Int) -> Int
get i j v = V.unsafeIndex (V.unsafeIndex v  i) j


tab :: Int -> Int -> (Int -> Int -> a) -> Vector (Vector a)
tab n m f = V.generate n (\i -> V.generate m (f i))

knapsack :: Int -> Vector Int -> Vector Int -> Int
knapsack m vs ws = get n m s
  where
    n = V.length vs
    getV i = vs V.! i
    getW i = ws V.! i
    s = tab (n + 1) (m + 1) (\i j ->
      if i == 0 || j == 0 then 0
      else if ( j - (getW (i - 1)) >= 0 )
        then
          max (get (i - 1) (j) s ) ( get (i - 1) (j - getW (i - 1)) s + getV (i - 1))
        else get (i - 1) (j) s
      )

{-
knapsack 9 ( V.fromList [3,3,5,3,7]) (V.fromList [1,2,4,2,3])
ws = V.fromList [1,2,4,2,3]
vs = V.fromList [3,3,5,3,7]
m = 9
should get 16,
test refactoring
-}
