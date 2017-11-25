module Circular() where

-- http://izzycecil.com/posts/2015-07-29-circular.html

--replace every element of a list with the minimum element of that list.

-- do not understand, FIXME: try to understand!

rplc :: Ord a => [a] -> [a]
rplc [] = []
rplc ss = res
  where
    (res, m'') = rplc' ss m''
    rplc' [x] m     = ([m], x)
    rplc' (x:xs) m = (m:ys, min x m')
      where
        (ys, m') = rplc' xs m
