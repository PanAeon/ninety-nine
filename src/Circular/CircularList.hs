module CircularList() where

-- replace list elements by minimum value

-- replace :: (Num a, Ord a) => [a] -> [a]
-- replace [] = []
-- replace xs = xs'
--    where
--      --replace' :: (Num a, Ord a) => [a] -> (a, [a])
--      replace' []     = (Nothing, [])
--      replace' (y:ys) = let m'' = maybe y id $ fmap (\x -> min x y) m'
--                            (m''', xs''') = replace ys
--                        in (Just m'', m'': (replace ys))
--      (m', xs')       = replace' xs


replace :: (Num a, Ord a) => [a] -> [a]
replace xs = xs'
   where
     (xs', m') = minList xs m'

trace :: (a -> c -> (b,c)) -> a -> b
trace f a = b
    where (b,c) = f a c

repminList :: (Num a, Ord a) => [a] -> [a]
repminList = trace minList


minList' :: (Num a, Ord a) => [a] -> a -> ([a], a)
minList' [x] m    = ([m], x)
minList' (x:xs) m = let
                     (xs', m') = minList' xs m
                   in (m:xs', min x m')




minList :: (Num a, Ord a) => [a] -> a -> ([a], a)
minList [x] m = ([m], x)
minList (x:xs) m = let (xs', m') = minList xs m
                   in (m : xs', min x m')
