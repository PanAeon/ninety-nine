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
   gray
) where

import Control.Monad(forM, forM_)
import Control.Monad.Loops(unfoldrM)
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
gray:: Int -> [String]
gray = undefined
