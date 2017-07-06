module Ex54to60 (
    completelyBalancedTree
  , hbalTree
) where

import Data.List(group, sort, findIndex)
import Data.Maybe(fromJust)

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

-- mapM_  prettyPrint $ hbalTree 3 'x'
prettyPrint :: (Show a) => Tree a -> IO ()
prettyPrint = prettyPrint' 0

prettyPrint' ::(Show a) => Int -> Tree a -> IO ()
prettyPrint' n Empty = putStrLn $ format' n "."
prettyPrint' n (Branch x Empty Empty) = putStrLn (format' n $ show x)
prettyPrint' n (Branch x l r) =
   putStrLn (format' n "(") >>= (\_ ->
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

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n = let (q, r) = (n - 1) `quotRem` 2
             in [q..q+r] >>= (\i ->
                cbalTree i >>= (\l ->
                  cbalTree (n - i - 1 ) >>= (\r ->
                    return $ Branch 'x' l r
                  )
                )
             )

-- Problem 56  Symmetric binary trees
symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch x l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch xl ll rl) (Branch xr lr rr) =  (mirror ll rr) && (mirror rl lr)
mirror _ _ = False

-- Problem 57   Binary search trees (dictionaries)

{-Use the predicate add/3, developed in chapter 4 of the course,
 to write a predicate to construct a binary search tree from a list of integer numbers. -}

buildTree :: [Int] -> Tree Int
buildTree [] = Empty
buildTree (x:xs) = insert x  (buildTree xs)
                 where
                   insert x Empty = Branch x Empty Empty
                   insert x b@(Branch y l r) | x < y = Branch y (insert x l) r
                                           | x > y = Branch y l (insert x r)
                                           | x == y = b

-- Problem 58 Generate-and-test paradigm

{- Apply the generate-and-test paradigm to construct all symmetric,
completely balanced binary trees with a given number of nodes. -}
balancedSymmetricTree :: Int -> [Tree Char]
balancedSymmetricTree 0 = []
balancedSymmetricTree 1 = [leaf 'x']
balancedSymmetricTree n = filter (\t -> isBalanced t && symmetric t) trees
                           where
                             isBalanced Empty = True
                             isBalanced (Branch _ l r) = isBalanced l && isBalanced r &&
                                         abs ((maxLength l) - (maxLength r)) <= 1
                             (q, r) = (n - 1) `quotRem` 2
                             trees =  [Branch 'x' left right | i <- [q .. q + r],
                                                               left <- cbalTree i,
                                                               right <- cbalTree (n - i - 1)]
symCbalTrees n = if n `mod` 2 == 0 then [] else
    [ Branch 'x' t (reverseTree t) | t <- cbalTree (n `div` 2)]

reverseTree Empty = Empty
reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)


-- Problem 59 Height-balanced binary Tree
hbalTree :: Int -> a -> [Tree a]
hbalTree 0 _ = [Empty]
hbalTree 1 x = [Branch x Empty Empty]
hbalTree h x = [Branch x l r  |
                (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
                l <- hbalTree hl x,
                r <- hbalTree hr x ]


hbalTree' :: a -> Int -> [Tree a]
hbalTree' _ 0 = [Empty]
hbalTree' x 1 = [Branch x Empty Empty]
hbalTree' x h = [Branch x l r  |
                                (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
                                l <- hbalTree hl x,
                                r <- hbalTree hr x ]


-- Construct height-balanced binary trees with a given number of nodes

minNodesWrong :: Int -> Int
minNodesWrong 0 = 0
minNodesWrong 1 = 1
minNodesWrong n = 2 * minNodesWrong (n - 1)

minNodesSeq :: [Int]
minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)
minNodes' = (minNodesSeq !!)

--maxHeight :: Int -> Int
--maxHeight n = head $ dropWhile (\i -> minNodes (i+1) <= n) [1..]
-- TODO: do it proper
hbalTreeNodes :: Int -> a -> [Tree a]
hbalTreeNodes 0 _ = [Empty]
hbalTreeNodes n x = concatMap toFilteredTrees [minHeight .. maxHeight]
              where
                  toFilteredTrees = filter ((n ==) . countNodes) . hbalTree' x
                  -- Similar to the Fibonacci sequence but adds 1 in each step.
                  minNodesSeq :: [Int]
                  minNodesSeq = 0:1:zipWith ((+).(1+)) minNodesSeq (tail minNodesSeq)
                  minNodes = (minNodesSeq !!)

                  minHeight :: Int
                  minHeight = ceiling $ logBase 2 $ fromIntegral (n+1)
                  maxHeight:: Int
                  maxHeight = (fromJust $ findIndex (>n) minNodesSeq) - 1

                  countNodes Empty = 0
                  countNodes (Branch _ l r) = countNodes l + countNodes r + 1
-- hbalTreeNodes n x = [Branch x l r  |
--                 (hl, hr) <- [(h-2, h-1), (h-1, h-1), (h-1, h-2)],
--                 l <- hbalTree hl x,
--                 r <- hbalTree hr x ]
--
--                 cbalTree :: Int -> [Tree Char]
--                 cbalTree 0 = [Empty]
--                 cbalTree n = let (q, r) = (n - 1) `quotRem` 2
--                              in [q..q+r] >>= (\i ->
--                                 cbalTree i >>= (\l ->
--                                   cbalTree (n - i - 1 ) >>= (\r ->
--                                     return $ Branch 'x' l r
--                                   )
--                                 )
--                              )
