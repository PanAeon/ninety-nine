module Manatee.LazyLambda
    (
    ) where


--  https://hackhands.com/modular-code-lazy-evaluation-haskell/
-- "Little Lambda decided to clean up his room at a later time."


{-
Reducing graph,

normal form - irreducible, finite, without cycles.
ones = 1 : ones -- not in normal form, because of a cycle. But in WHNF
WHNF - topmost node is a constructor (or a lambda abstraction)

then   any graph that is not in WHNF is called an unevaluated expression or thunk.

-}

foldl_ :: (a -> b -> a) -> a -> [b] -> a
foldl_ f z [] = z
foldl_ f z (x:xs) = foldl_ f (f z x) xs

{-
foldl (+) 0 [1..100]
foldl (+) (0 + 1) [2..100]
foldl (+) ((0 + 1) + 2) [3..100]
foldl (+) (((0 + 1) + 2) + 3) [4..100]
foldl (+) ((((0 + 1) + 2) + 3) + 4) [5..100]
...
foldl (+) (((((0 + 1) + 2) + 3) + 4) + ...) [] =>
(((((0 + 1) + 2) + 3) + 4) + ...)
-}

foldl_' :: (a -> b -> a) -> a -> [b] -> a
foldl_' f z [] = z
foldl_' f z (x:xs) = seq z' foldl_' f z' xs
  where
    z' = f z x

foldr_ :: (b -> a -> a) -> a -> [b] -> a
foldr_ f z [] = z
foldr_ f z (x:xs) = f x (foldr_ f z xs)

{-
foldr (+) 0 [1..100]
1 + (foldr (+) 0 [2..100])
1 + (2 + foldr (+) 0 [3..100])
...
1 + (2 + 3 +(... + foldr (+) 0 [])) =>
1 + (2 + 3 +(... + 0))
-}


-- Small keeps reference to large !!
--let small' = fst (small, large) in … small' …

{- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
 minimum = head . sort
 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -}

and' :: [Bool] -> Bool
and' = foldr (&&) True

prefix :: Eq a => [a] -> [a] -> Bool
prefix xs ys = and (zipWith (==) xs ys)

enumFrom' n = n `seq` (n : enumFrom' (n+1))


better a x = 1/2*(x + a/x)

within eps (x:y:ys) =
    if abs (x-y) <= eps then y else within eps (y:ys)

-- looks like taken from "Y functional programming matter", and yes it is
squareRoot a = within (1e-14) $ iterate (better a) (a/2)

-- User-defined control structures


------------------------
-- denotes 1 : 2 : ⊥
hangs = filter (< 3) [1..]

data Sign = Rock | Paper | Scissors deriving (Eq, Show)

beat :: Sign -> Sign
beat Rock     = Paper
beat Paper    = Scissors
beat Scissors = Rock

points :: (Sign, Sign) -> (Int,Int)
points (x,y)
    | x == y      = (0,0)   -- draw
    | x == beat y = (1,0)
    | y == beat x = (0,1)

rounds :: (Strategy, Strategy) -> [(Sign, Sign)]
rounds (player1, player2) = zip signs1 signs2
    where
    signs1 = player1 (sync signs2 signs1)
    signs2 = player2 (sync signs1 signs2)



match :: Int -> (Strategy, Strategy) -> (Int, Int)
match n =
    pair sum . unzip . map points . take n . rounds
    where
    pair f (x,y) = (f x, f y)

type Strategy = [Sign] -> [Sign]

alwaysPaper :: Strategy
alwaysPaper signs = repeat Paper


whatYouDid :: Strategy
whatYouDid signs = Paper : map beat signs

cheat :: Strategy
cheat signs = map beat signs

sync :: [a] -> [b] -> [a]
sync []     []     = []
sync (x:xs) (_:ys) = x : sync xs ys
