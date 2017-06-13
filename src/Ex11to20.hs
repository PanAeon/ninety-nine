module Ex11to20
    (   encodeModified
      , decodeModified
      , encodeDirect
      , dupli
      , repli
      , drop'
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

drop' :: Int -> [a] -> [a]
drop' n xs = undefined
        where z0 = fromList xs
