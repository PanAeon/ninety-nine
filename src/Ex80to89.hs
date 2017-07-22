module Ex70to73 (

) where

import Data.List(group, sort, findIndex, intersect, unfoldr, intersperse, nubBy, nub,
                    (\\))
import Data.Maybe(fromJust, isJust)
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Monad.Loops(iterateWhile, unfoldM)
import Control.Applicative(liftA, Alternative, many)
import Control.Monad(ap, MonadPlus, mplus)
import qualified Control.Applicative as App


data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

data Friendly a = Edge [(a, a)] deriving (Show, Eq)

--  Problem 80
graphToAdj :: (Eq a) => Graph a -> Adjacency a
graphToAdj (Graph xs links) = Adj $ fmap g xs
             where
               g x = (x, links >>= (findAdj x))
               findAdj x (a, b) | x == a    = [b]
                                | x == b    = [a]
                                | otherwise = []

adjToGraph :: (Eq a) => Adjacency a -> Graph a
adjToGraph (Adj xs)= Graph zs (nubBy bar (concat foo))
              where
                (zs, foo) = unzip $ map f xs
                f (a,ys) = (a, map (\x -> (a,x)) ys)
                bar (a,b) (c,d) = (a == c && b == d) || (a == d && b == c)


graphToFri :: (Eq a) => Graph a -> Friendly a
graphToFri (Graph [] _)  = Edge []
graphToFri (Graph xs ys) = Edge (ys ++ zip g g)
    where
       g = filter (\x -> all (\(a, b) -> x /= a && x /= b) ys) xs


-- TODO: if you want you can use arrows or smth to deal with tuples
friToGraph :: (Eq a) => Friendly a -> Graph a
friToGraph (Edge xs) = Graph nodes connections
             where
               connections = filter (\t -> not $ fst t == snd t) xs
               nodes = nub (xs >>= (\t -> [fst t, snd t]))

adjToFri :: (Eq a) => Adjacency a -> Friendly a
adjToFri = graphToFri . adjToGraph

friToAdj :: (Eq a) => Friendly a -> Adjacency a
friToAdj = graphToAdj . friToGraph

--  Problem 81
--  Path from one node to another one
--  returns all the acyclic paths from a to b.

-- undirected paths, but I believe that's fine
paths :: (Eq a) => a -> a -> Adjacency a -> [[a]]
paths a b  (Adj adj) = paths' a [a]
    where
      getAdj x = fromJust $ lookup  x adj
      paths' x visited = if x == b then
                            [[x]]
                         else
                            fmap (x:) $ concatMap (\y -> paths' y (x:visited)) (getAdj x \\ visited)


--  Problem 82

-- Cycle from a given node

{- Write a predicate cycle(G,A,P) to find a closed path (cycle)
   P starting at a given node A in the graph G.
   The predicate should return all cycles via backtracking.
-}
