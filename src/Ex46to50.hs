module Ex46to50 (
   not',
   and',
   or',
   nand',
   nor',
   xor',
   impl',
   equ',
   table,
   tableN,
   gray,
   huffman
) where

import Control.Monad(forM, forM_)
import Control.Monad.Loops(unfoldrM)
import qualified Data.PQueue.Prio.Min as PQ
-- Logic and Codes

-- Problem 46
--  Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- EX: table (\a b -> (and' a (or' a b)))
not' :: Bool -> Bool
not' True = False
not' False = True

and' :: Bool -> Bool -> Bool
and' True True   = True
and' _ _         = False


or' :: Bool -> Bool -> Bool
or' False False = False
or' _     _     = True

nand' :: Bool -> Bool -> Bool
nand' True True = False
nand' _    _    = True

nor' :: Bool -> Bool -> Bool
nor' False False = True
nor' _ _         = False

xor' :: Bool -> Bool -> Bool
xor' True True   = False
xor' False False = False
xor' _     _     = True

impl' :: Bool -> Bool -> Bool
impl' True False = False
impl' _    _     = True

equ' :: Bool -> Bool -> Bool
equ' True True = True
equ' False False = True
equ' _     _     = False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = sequence_ $ map printLine (table' f)
          where
            printLine (a,b,c) = putStrLn $ show a ++ " " ++ show b ++ " " ++ show c


table' :: (Bool -> Bool -> Bool) -> [(Bool, Bool, Bool)]
table' f = do
      a <- [True, False]
      b <- [True, False]
      return (a, b, f a b)

table'' :: (Bool -> Bool -> Bool) -> IO ()
table'' f = mapM_ putStrLn [show a ++ " " ++ show b ++ " " ++ show (f a b)
                                | a <- [True, False], b <- [True, False]]

infixl 4 `or'`
infixl 6 `and'`
infixl 7 `equ'`

-- Problem 48
-- generalize to N variables (use list !!! )
-- EX: tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)

tableN :: Int -> ([Bool] -> Bool) -> IO ()
tableN n f = forM_ ys printLine
         where
           xs = [True, False]
           ys = unfoldrM (\m -> if m == 0 then return Nothing else map (\y -> Just(y, m - 1)) xs ) n
           printLine xs = putStrLn $ (foldr (\b s -> s ++ " "++ show b)  "" xs)  ++ " " ++ (show $ f xs)
-- TODO: contemplate the beauty! unfodrM used right! see also replicateM which does the same thing


-- Problem 49 Gray Codes!
-- TODO: understand how they are built https://en.wikipedia.org/wiki/Gray_code
gray:: Int -> [String]
gray 1 = ["0","1"]
gray n = map ("0"++) xs ++ map ("1"++) (reverse xs)
         where
           xs = gray (n - 1)


data Tree a = Nil | Node (Maybe a) (Tree a) (Tree a)

-- Problem 50 Huffman codes
-- TODO: state monad ?? create/analyze heaps?
-- TODO: understand algorithm!!
huffman :: [(Char,Int)] -> [(Char, String)]
huffman xs = walkTree "" $ snd $ buildTree pq
        where
          pq = PQ.fromList $ map (\it -> (snd it, Node (Just (fst it)) Nil Nil) ) xs
          buildTree pq | PQ.size pq == 1 = PQ.findMin pq
                       | otherwise = let
                                      ((pa, a), pq') = PQ.deleteFindMin pq
                                      ((pb, b), pq'') = PQ.deleteFindMin pq'
                                      pq''' = PQ.insert (pa+pb) (Node Nothing a b) pq''
                                     in
                                      buildTree pq'''
          swap (a,b) = (b,a)
          walkTree _ Nil = []
          walkTree prefix (Node (Just a) Nil Nil) = [(a, reverse prefix)]
          walkTree prefix (Node Nothing l r) = (walkTree ("0"++prefix) l) ++
                                                (walkTree ("1"++prefix) r)
--1) Build a Huffman Tree from input characters.
-- 2) Traverse the Huffman Tree and assign codes to characters.
