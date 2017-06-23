module Ex11to20
    (   encodeModified
      , decodeModified
      , encodeDirect
      , dupli
      , repli
      , drop'
      , split
      , slice
      , rotate
      , removeAt'
      , pack
      , MList(Single, Multiple)

    ) where




import Control.Monad
import Control.Applicative
import Control.Arrow((&&&))
import Data.List(tails, group)
import Data.Maybe(listToMaybe)
import Data.List.Zipper

{-
1 Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates
it is simply copied into the result list.
 Only elements with duplicates are transferred as (N E) lists.

Example:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data MList a = Single a | Multiple Int a deriving (Eq,Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- TODO: List comprehensions
-- FIXME: Unit tests!!!
encodeModified :: (Eq a) => [a] -> [MList a]
encodeModified = map enc . pack
                 where
                   enc :: [a] -> MList a
                   enc [x] = Single x
                   enc xs    =  ($ xs) $ Multiple <$> length <*> head

decodeModified:: (Eq a) => [MList a] -> [a]
decodeModified xs = xs >>= unpack
                    where
                        unpack (Single a) = [a]
                        unpack (Multiple n a) = replicate n a

encodeDirect:: (Eq a) => [a] -> [MList a]
encodeDirect xs = encodeZ 0 (fromList xs)

-- TODO: generic foldrz ?? -- just pass z !
encodeZ :: (Eq a) =>  Int -> Zipper a -> [MList a]
encodeZ n z = case (safeCursor z, (safeCursor $ right z))
              of (Nothing, _) -> []
                 (Just x, Nothing) -> produce x
                 (Just x, Just y) | x == y -> encodeZ (n+1) (right z)
                                  | otherwise -> produce x ++ encodeZ 0 (right z)
              where
                produce a = if (n == 0) then [Single a] else [Multiple (n+1) a]

encodeZ' n z | endp z   = []
            | (endp (right z)) && (n == 0) = [Single (cursor z)]
            | (endp (right z)) && (n /= 0) = [Multiple (n+1) (cursor z)]
            | cursor z == (cursor $ right z) = encodeZ (n+1) (right z)
            | cursor z /= (cursor $ right z) && (n == 0) = [Single (cursor z)] ++ encodeZ' 0 (right z)
            | cursor z /= (cursor $ right z) && (n /= 0) = [Multiple (n+1) (cursor z)] ++ encodeZ' 0 (right z)


encodeDirect'' xs = reverse $ snd $ foldlz reducer (0, []) z0
              where
                reducer (n, xs) z | endp (right z) = (0, (produce n (cursor z)) ++ xs)
                                  | (cursor z) == (cursor $ right z) = (n+1, xs)
                                  | otherwise  = (0, (produce n (cursor z)) ++ xs)
                z0 = fromList xs
                produce n a = if (n == 0) then [Single a] else [Multiple (n+1) a]


dupli:: [a] -> [a]
dupli = concatMap (replicate 2)

dupli' = (<**> [id,id]) -- TODO: what is this shit??

repli :: Int -> [a] -> [a]
repli n = concatMap (replicate n)

-- FIXME: check this shit
repli' ::  Int -> [a] -> [a]
repli' =  concatMap . replicate

{-
6 Problem 16

(**) Drop every N'th element from a list.
-}

drop'' :: Int -> [a] -> [a]
drop'' n xs = toList $ fst $ foldlz f (z0, 1) z0
        where
          z0 = fromList xs
          f (z, m) z1 | (mod m n) == 0 = (delete z, m + 1)
                      | otherwise = (right z, m + 1)
-- TODO: comprehensions
drop' :: Int -> [a] -> [a]
drop' n = toList . (foldl (\z i ->
                 if mod i n == 0 then
                   delete z
                 else
                  right z
              ) <$> fromList <*> idxs)
        where
          idxs = flip take [1..] . length
-- n is a length of first list
-- do not use any predefined predicates
split :: Int -> [a] -> ([a], [a])
split n []     = ([], [])
split 0 xs     = ([], xs)
split n (x:xs) = let (a,b) = split (n - 1) xs
                 in (x:a, b)

-- TODO: how the fuck it works?
split' :: [a] -> Int -> ([a], [a])
split' (x:xs) n | n > 0 = (:) x . fst &&& snd $ split' xs (n - 1)
split' xs _             = ([], xs)

{-
Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements
between the i'th and k'th element of the original list (both limits included).
 Start counting the elements with 1.
-}

slice:: Int -> Int -> [a] -> [a]
slice i k = take (k - i + 1) . (drop (i - 1))
-- slice = ap (flip . (((.) . take) .) . flip flip 1 . ((+) .) . subtract) (drop . subtract 1)


-- Rotate a list N places to the left.

rotate:: Int -> [a] -> [a]
rotate n xs = zs ++ ys
         where
           l = length xs
           r = if (n > 0 ) then n `mod` l else (l+n) `mod` l
           (ys, zs) = ( take r xs, drop r xs)

rotate':: Int -> [a] -> [a]
rotate' n xs = take len . drop (n `mod` len) . cycle $ xs
      where len = length xs

{- Problem 20 remove kth element from the list -}
-- simple
removeAt:: Int -> [a] -> [a]
removeAt k = (++) <$> take k <*> drop (k + 1)

-- zippers ! yay
removeAt':: Int -> [a] -> (a, [a])
removeAt' k xs = ((,) <$> cursor <*> (toList . delete) ) $ foldl (flip ($)) (fromList xs) (replicate (k-1) right)
