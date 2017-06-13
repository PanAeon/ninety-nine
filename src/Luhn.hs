
module Luhn(check,baz) where

import Data.List(unfoldr)

-- TODO: parse CC number from string, possible ****-****-****-**** format
-- FIXME: convert to pointfree, remove $ in favour of something less backward
check :: [Int] -> Bool
check xs = (==0) $ (`mod` 10) $ sum $ unfoldr bar (True, reverse xs)
  where
    bar (_,[]) = Nothing
    bar (False, y:ys)  = Just (if 2*y > 9 then 2*y-9 else 2*y, (True, ys))
    bar (True,y:ys)      = Just(y, (False, ys))

baz :: (Bool, [Int]) -> Maybe (Int, (Bool, [Int]))
baz  (_,[]) = Nothing
baz  (False, y:ys)  = Just (if 2*y > 9 then 2*y-9 else 2*y, (True, ys))
baz  (True,y:ys)      = Just(y, (False, ys))

-- 1. Double every second digit from the right
-- 2. add the digits from the doubled and original digits that are not doubled
-- 3. div 10 should=== 10
