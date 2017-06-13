module Lib
    ( myButLast,
      lastbut1,
      myLength,
      myReverse
    ) where




import Control.Monad
import Control.Applicative
import Control.Arrow((&&&))
import Data.List(tails, group)
import Data.Maybe(listToMaybe)



myButLast:: [x] -> x
myButLast [] = error "empty list"
myButLast (_:[]) = error "list size 1"
myButLast (x:[_]) = x
myButLast (_:xs) = myButLast xs


lastbut1 :: Foldable f => f a -> Maybe a
lastbut1 = fst . foldl (\(_,b) x -> (b, Just x)) (Nothing, Nothing)


myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = myLength xs + 1

myLength' :: [a] -> Int
myLength' = foldl (const . (+1)) 0

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom xs = xs == reverse xs

isPalindrome''' :: (Eq a) => [a] -> Bool
isPalindrome''' = Control.Monad.liftM2 (==) id reverse

isPalindrome'''' :: (Eq a) => [a] -> Bool
isPalindrome'''' = (==) Control.Applicative.<*> reverse


element_at :: Foldable f => Int -> f a -> Maybe a
element_at n = snd . foldl inner (n, Nothing)
         where
            inner a@(_, Just _) _ =  a
            inner (0, _) a = (0, Just a)
            inner (x, _) _ = (x - 1, Nothing)

isPalindrome1 :: (Eq a) => [a] -> Bool
isPalindrome1 = (uncurry (==) . (id &&& reverse))


reduceSuccessive :: Eq x => [x] -> [x]
reduceSuccessive xs = (\(a,b) -> if (Just a) == (listToMaybe b) then [] else [a]) =<< (zip xs (tail $ tails xs))


data NestedList a = Elem a | List [NestedList a] deriving Show

-- TODO: with fold or higher order combinators?
-- TODO: unit-tests, stack project?
myFlatten :: NestedList a -> [a]
myFlatten (Elem a)  = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = (myFlatten x) ++ (myFlatten (List xs))

flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x

flatten'' (Elem x) = return x
flatten'' (List x) = flatten'' =<< x


-- Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress = map head . group

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = map ((,) <$> length <*> head) . pack
