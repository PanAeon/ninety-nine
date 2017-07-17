{-# LANGUAGE DeriveFunctor #-}
module Ex61to69 (

) where

import Data.List(group, sort, findIndex)
import Data.Maybe(fromJust)
import Ex54to60(prettyPrint, Tree(Empty, Branch))
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
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


-- looks like indxd construction may be the fastest one , but ...
-- cont? zipper?

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

-- ?? arrow?
enqueue :: Seq.Seq a -> a -> Seq.Seq a
enqueue = (Seq.|>)

dequeue :: Seq.Seq a -> (Seq.Seq a, Maybe a) -- wrong signature Maybe (Seq a, a)
dequeue xs = if null xs then
               (xs, Nothing)
             else (Seq.drop 1 xs, Just $ xs `Seq.index` 0)

deq :: Seq.Seq a -> (a, Queue a)
deq xs = ( xs `Seq.index` 0, Seq.drop 1 xs)

type Queue = Seq.Seq
-- todo: isLeaf? NO?
isCompleteBinaryTree :: Tree a -> Bool
isCompleteBinaryTree tree = fst $ levelOrderTraversal f (True,"f") tree
                            where
                              f :: (Bool, String) -> (Tree a) -> (Bool, String)
                              f (False, _) _ = (False, "e")
                              f (_, "f") Empty = (True, "e")
                              f (_, "l") Empty = (True, "e")
                              f (_, "e") Empty = (True, "e")
                              f (_, "f") (Branch _ (Branch _ _ _) (Branch _ _ _)) = (True, "f")
                              f (_, "f") (Branch _ Empty (Branch _ _ _)) = (True, "l")
                              f (_, "f") (Branch _ Empty Empty) = (True, "l")
                              f (_, "f") (Branch _  (Branch _ _ _) Empty) = (True, "l")
                              f (_, "l") (Branch _ Empty (Branch _ _ _)) = (True, "l")
                              f (_, "l") (Branch _  (Branch _ _ _) Empty) = (True, "l")
                              f (_, "l") (Branch _ Empty Empty) = (True, "l")

                              f _ _ = (False, "e")
                              isInternal (Branch _ (Branch _ _ _) (Branch _ _ _)) = True
                              isInternal _ = False
                              isBranch (Branch _ _ _ ) = True
                              isBranch _ = False



levelOrder :: (b -> (Tree a) -> b) -> b -> Tree a -> Queue (Tree a) -> b
levelOrder f z Empty q | Seq.null q = f z Empty --
                       | otherwise  = (uncurry $ levelOrder f (f z Empty)) $ deq q

levelOrder f z b@(Branch x left right) q = ((uncurry $ levelOrder f (f z b)) $ deq q'')
              where
                q'  = enqueue q left
                q'' = enqueue q' right

-- FIXME: Foldable intance for Tree, how to choose between different typeclasses?
-- how to deal with leaves?
levelOrderTraversal :: (b -> (Tree a) -> b) -> b -> (Tree a) -> b
levelOrderTraversal f z tree = levelOrder f z tree Seq.empty
-- good, now we only need level-order fold and level order unfold, with Empty/Branch as inputs

              --   where
              -- --    levelOrder Empty =
              -- --    levelOrder (Branch x left right) =
              --     inOrder Empty = ["e"]
              --     inOrder (Branch _ Empty Empty) = ["l"]
              --     inOrder (Branch )


genTree :: Int -> Tree String
genTree 0 = Empty
genTree n = Branch (show n) (genTree (n-1)) (genTree (n - 1))

collectLevelOrder :: (Show a) => Tree a -> String
collectLevelOrder = levelOrderTraversal f ""
        where
          f s Empty = s ++ " e"
          f s (Branch x _ _) = s ++ " " ++ (show x)


data TBNode = TBNode (Maybe TBNode) Bool deriving(Show)

-- queue should already have 1 node
buildTBuilder' :: Int -> Queue TBNode -> [TBNode]
buildTBuilder' 0 q = Fldbl.foldr (:) [] q
buildTBuilder' n q = if n == 1 then
                        buildTBuilder' 0 $ enqueue q' (TBNode (Just p) True)
                     else
                        buildTBuilder' (n - 2) q'''
            where
              (p, q') = deq q
              q'' = enqueue q' (TBNode (Just p) True)
              q''' = enqueue q'' (TBNode (Just p) False)


convertToTree :: [TBNode] -> Tree Char
convertToTree = undefined -- parent comparison without uuid is expensive!!


cbt :: Int -> Tree Char
cbt 0 = Empty
cbt n = build 1
  where
    m = n + 1
    build i | i < m     = Branch 'x' (build (i * 2)) (build (i * 2 + 1))
            | otherwise = Empty


-- fooh,  Problem 64
-- TODO: could be done in single pass (easy)
layout1 :: Tree a -> Tree (a, (Int, Int))
layout1 tree = snd $  annotateX (annotateY tree 1) 0
            where
              annotateY Empty _ = Empty
              annotateY (Branch x l r) n = Branch (x, n) (annotateY l (n +1)) (annotateY r (n +1))
              annotateX :: Tree (a,Int) -> Int -> (Int, Tree (a, (Int, Int)))
              annotateX Empty  m = (m, Empty)
              annotateX (Branch (a,y) l r) m = (n', Branch (a, (n+1, y)) l' r')
                                           where
                                             (n,l') = annotateX l m
                                             (n', r') = annotateX r (n+1)


-- Problem 65
layout2 :: Tree a -> Tree (a, (Int, Int))
layout2 tree = annotate tree xp 1 sep1
    where
      maxHeight Empty = 0
      maxHeight (Branch _ l r) = 1 + max (maxHeight l) (maxHeight r)
      treeHeight = maxHeight tree

      leftDepth Empty = 0
      leftDepth (Branch  _ l _) = 1 + leftDepth l

      leftD = leftDepth tree

      sep1 = 2 ^ (treeHeight - 2)

      xp =  2^(treeHeight-1) - 2^(treeHeight-leftD) + 1

      annotate :: Tree a -> Int -> Int -> Int -> Tree (a, (Int, Int))
      annotate Empty x y sep = Empty

      annotate (Branch a l r) x y sep =  Branch (a, (x, y)) l' r'
                      where
                        l' = annotate l (x - sep) (y + 1) (sep `div` 2)
                        r' = annotate r (x + sep) (y + 1) (sep `div` 2)







tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )


-- Problem 66
type Pos = (Int, Int)

layout3 ::  Tree a -> Tree (a, Pos)
layout3 = snd . build  1 1
        where
          build  shift y Empty = (shift, Empty)
          build  shift y (Branch a l r) = undefined
                 where
                  (shift', l') = build shift (y+1) l
                  -- let gap = 2

                  -- while { shift'' = shift' + gap; (rightEdge shift'' r ) overlaps l'}
                  --       { gap = gap * 2; }
                  -- build (shift' + gap') r
                  --
                  -- return Parent(shif't + gap'/2 y)
                  -- l' = build l   -- Set Pos
                  -- while overlap
                  -- increase parent gap * 2
                  --
                  --right-edge = leftEdge r parentPos
                  --
                  -- buildr r
          -- FIXME: this is wrong, need *all left* nodes, note just left nodes
          overlaps :: Tree (a, Pos) -> Tree (a, Pos) -> Bool
          overlaps Empty _ = False
          overlaps _ Empty = False
          overlaps (Branch (_, (x1,_)) _ r') (Branch (_, (x2,_)) l' _) | x1 >= x2 = True
                                                                       | otherwise = False
