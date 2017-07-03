module Ex54to60 (
  completelyBalancedTree
) where

import Data.List(group, sort)

data Tree a = Empty | Branch a (Tree a) (Tree a)
                deriving (Show, Eq, Ord)

leaf x = Branch x Empty Empty

-- Problem 55 Construct completely balanced binary trees
-- TODO: efficient zipper version, backtracking version, fold version!!!, CONT version!!
completelyBalancedTree :: Int -> [Tree Char]
completelyBalancedTree 0 = []
completelyBalancedTree 1 = [leaf 'x']
completelyBalancedTree n = rmdups $ filter isBalanced  ( completelyBalancedTree (n - 1) >>= insertElem )
                           where
                             isBalanced Empty = True
                             isBalanced (Branch _ l r) = isBalanced l && isBalanced r &&
                                         abs ((maxLength l) - (maxLength r)) <= 1


maxLength Empty = 0
maxLength (Branch _ l r) = 1 + max (maxLength l) (maxLength r)


insertElem Empty = [Branch 'x' Empty Empty]
insertElem (Branch x l r) = map (\l' ->
                               Branch x l' r
                               ) (insertElem l) ++ map (\r' ->
                              Branch x l r'
                               ) (insertElem r)


prettyPrint :: Tree Char -> IO ()
prettyPrint = prettyPrint' 0

prettyPrint' :: Int -> Tree Char -> IO ()
prettyPrint' n Empty = putStrLn $ format' n "."
prettyPrint' n (Branch x Empty Empty) = putStrLn (format' n $ show x)
prettyPrint' n (Branch x l r) = putStrLn (format' n "(") >>= (\_ ->
                                     putStrLn (format' n $ show x)) >>= (\_ ->
                                     prettyPrint' (n+1) l) >>= (\_ ->
                                     prettyPrint' (n+1) r
                                  ) >>= (\_ -> putStrLn $ format' n ")")

format' n c = (replicate (n*4) ' ') ++ (c)

genFull' 0 = Empty -- ok, full tree
genFull' n = Branch 'x' (genFull' (n - 1)) (genFull' (n - 1))

completelyBalancedTree' :: Int -> [Tree Char]
completelyBalancedTree' n = undefined
                        where
                          hFull = floor (log (fromIntegral n) / log 2.0) :: Int
                          full = genFull' hFull
                          nLast = n - (2^hFull - 1)
                          insert' Empty = [leaf 'x']

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
