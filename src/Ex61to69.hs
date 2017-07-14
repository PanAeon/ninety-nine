{-# LANGUAGE DeriveFunctor #-}
module Ex61to69 (

) where

import Data.List(group, sort, findIndex)
import Data.Maybe(fromJust)
import Ex54to60(prettyPrint, Tree(Empty, Branch))
import qualified Data.Sequence as Seq




-- count leaves
-- Count the leaves of a binary tree

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

--  Problem 61A
-- Collect the leaves of a binary tree in a list

leaves :: Tree a -> [Tree a]
leaves Empty = []
leaves x@(Branch _ Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

--  Problem 62

-- Collect the internal nodes of a binary tree in a list

-- An internal node of a binary tree has either one
-- or two non-empty successors. Write a predicate internals/2 to collect them in a list.


internals :: Tree a -> [Tree a]
internals Empty = []
internals  (Branch _ Empty Empty) = []
internals x@(Branch _ l r) = x : internals l ++ internals r


-- Problem 62B

-- Collect the nodes at a given level in a list

{-
A node of a binary tree is at level N if the path from the root
to the node has length N-1. The root node is at level 1.
Write a predicate atlevel/3 to collect all nodes at a given level in a list.
-}

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 0 = [x]
atLevel (Branch _ l r) n = atLevel l (n - 1) ++ atLevel r (n - 1)


-- Problem 63

-- Construct a complete binary tree

{-
A complete binary tree with height H is defined as follows:

   The levels 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2**(i-1) at the level i)
   In level H, which may contain less than the maximum possible number of nodes,
   all the nodes are "left-adjusted". This means that in a levelorder tree traversal
   all internal nodes come first, the leaves come second, and empty
   successors (the nil's which are not really nodes!) come last.

Particularly, complete binary trees are used as data structures (or addressing schemes)
for heaps.

We can assign an address number to each node in a complete binary tree by enumerating
the nodes in level-order, starting at the root with number 1.
For every node X with address A the following property holds:
The address of X's left and right successors are 2*A and 2*A+1, respectively,
 if they exist. This fact can be used to elegantly construct a complete binary tree structure.

-}



height' :: Int -> Int
height' n = ceiling $ (logBase 2 (fromIntegral (n+1)))
-- FIXME: completeBinaryTree 7
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = insertLeft' full m
                       where
                         height = height' n
                         full = fullTree (height - 1)
                         m = n - (2^(height - 1) - 1) -- maybe height is right but
                         -- insert m nodes to the left       m is wrong


insertLeft' :: Tree Char -> Int -> Tree Char
insertLeft' tree n = fst $ ins tree n
            where
              ins tree 0 = (tree, 0)
              ins (Empty) n = error "shouldn't happen, right?"
              ins (Branch x Empty Empty) 1 = ((Branch x (Branch 'x' Empty Empty) Empty), 0)
              ins (Branch x Empty Empty) n = ((Branch x (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)), n-2)
              ins (Branch x left  right) n = let
                                               (l', m) = ins left n
                                               (r', m') = ins right m
                                             in
                                               ((Branch x l' r'), m')

insertLeft :: Tree Char -> Int -> Int -> Tree Char -- fuck backtrack
insertLeft b h 0 = b
insertLeft Empty _ _ = Branch 'x' Empty Empty
insertLeft b 0 n = error $ ">> b: " ++ (show b) ++ ", n: " ++ show n
insertLeft (Branch c l r) 1 1 = Branch 'x' (Branch 'x' Empty Empty) Empty
insertLeft (Branch c l r) 1 2 = Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)
insertLeft (Branch c l r) h n = Branch c (insertLeft l (h-1) n1) (insertLeft r (h-1) n2)
                                where
                                  n1 = min n (2^(h-1))
                                  n2 = max 0 (n - n1)

fullTree :: Int -> Tree Char
fullTree 0 = Empty
fullTree h = Branch 'x' (fullTree (h - 1)) (fullTree (h - 1))

enqueue :: Seq.Seq a -> a -> Seq.Seq a
enqueue = (Seq.|>)

dequeue :: Seq.Seq a -> (Seq.Seq a, Maybe a)
dequeue xs = if null xs then
               (xs, Nothing)
             else (Seq.drop 1 xs, Just $ xs `Seq.index` 0)

deq :: Seq.Seq a -> (a, Queue a)
deq xs = ( xs `Seq.index` 0, Seq.drop 1 xs)

type Queue = Seq.Seq

isCompleteBinaryTree :: Tree Char -> Bool
isCompleteBinaryTree tree = undefined

levelOrder :: Tree a -> Queue (Tree a) -> [a]
levelOrder Empty q | Seq.null q = []
                   | otherwise  = (uncurry levelOrder) $ deq q

levelOrder (Branch x left right) q = x : ((uncurry levelOrder) $ deq q'')
              where
                q'  = enqueue q left
                q'' = enqueue q' right

-- good, now we only need level-order fold and level order unfold, with Empty/Branch as inputs

              --   where
              -- --    levelOrder Empty =
              -- --    levelOrder (Branch x left right) =
              --     inOrder Empty = ["e"]
              --     inOrder (Branch _ Empty Empty) = ["l"]
              --     inOrder (Branch )