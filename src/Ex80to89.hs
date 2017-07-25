module Ex70to73 (

) where

import Data.List(group, sort, findIndex, intersect, unfoldr, intersperse, nubBy, nub,
                    (\\), delete, inits, tails, sortBy, sortOn)
import Data.Maybe(fromJust, isJust)
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Monad.Loops(iterateWhile, unfoldM)
import Control.Applicative(liftA, Alternative, many)
import Control.Monad(ap, MonadPlus, mplus)
import qualified Control.Applicative as App
import qualified Data.Map as Map

data Graph a = Graph [a] [(a,a)] deriving (Show, Eq)

data Adjacency a = Adj [(a, [a])] deriving (Show, Eq)

data Friendly a = Edge [(a, a)] deriving (Show, Eq)

data WeightedGraph a = WeightedGraph [a] [(a,a, Int)] deriving (Show, Eq)

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

cycle' :: (Eq a) => a -> Adjacency a -> [[a]]
cycle' a (Adj adj) = cycle'' a []
      where
        getAdj x = fromJust $ lookup x adj
        cycle'' x visited = fmap (x:) $ concatMap (\y -> cycle''' y foo) (getAdj x \\ visited)
                            where
                              foo = if x == a then visited else x:visited
        cycle''' x visited = if x == a then
                               [[a]]
                             else
                               cycle'' x visited
--Problem 83

--(**) Construct all spanning trees

{-
Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all spanning trees
of a given graph. With this predicate, find out how many spanning trees there are for
the graph depicted to the left. The data of this example graph can be found in the file
p83.dat. When you have a correct solution for the s_tree/2 predicate,
use it to define two other useful predicates: is_tree(Graph) and is_connected(Graph).
Both are five-minutes tasks!
-}

-- I don't know much about matrix bloody singularity and shit
-- so will construct the graph as a dork
-- TODO: this could be optimized, with at least checking connectedness smarter,
-- with dfs and shit
-- FIXME: should return graph itself if it is it's  own  spanning tree
bfSpanningTree :: (Eq a) => Graph a -> [Graph a]
bfSpanningTree = spanning
   where
     spanning g@(Graph nodes edges) = if null current then maybeg else current >>= spanning
        where
          current = filter isConnected $ map (\es -> Graph nodes es) (allButOne edges)
          maybeg = if isConnected g then [g] else []
     allButOne xs = if null xs then
                    []
                 else
                   init $ map (uncurry (++)) $ zip (inits xs) (map (drop 1) $ tails xs)
     rmEdge x (Graph nodes edges) =  Graph nodes (delete x edges)
     isConnected g@(Graph nodes edges) = all (canConnectToOther g) nodes
     canConnectToOther g@((Graph nodes _)) x = all (\y -> areConnected x y g) (delete x nodes)
     areConnected a b g = not $ null $ paths a b (graphToAdj g)

isTree :: (Eq a) => Graph a -> Bool
isTree g = (length st == 1) && (head st == g)
      where
        st = bfSpanningTree g

maxInt = 100000
fst' (a,b,c) = a
snd' (a,b,c) = b
thrd (a,b,c) = c

headOption [] = Nothing
headOption (x:_) = Just x
-- data WeightedGraph a = WeightedGraph [a] [(a,a, Int)] deriving (Show, Eq)
minimumSpanningTree:: (Eq a) => WeightedGraph a  -> WeightedGraph a
minimumSpanningTree g@(WeightedGraph nodes  edges) = spanning [] (map (\x -> (x, maxInt)) nodes) edges
      where
        spanning xs [] edges = WeightedGraph (map fst xs) edges
        spanning processed ((x,wx):xs) edges = spanning ((x,wx):processed) xs'' prunedEdges
          where
            edges' = map (putFirst x) $ filter (connected x) edges
            connected x (a,b,c) = x == a || x == b
            putFirst x (a,b,c) = if x == a then (a,b,c) else (b,a,c)
            xs' = map updateWeights xs
            updateWeights (a,w) = if (ew < w) then (a,ew) else (a,w)
              where
                (_, _, ew) = maybe (undefined, undefined, maxInt) id $  headOption $ filter (\y -> snd' y == a) edges'
            xs'' = sortOn snd xs'
            prunedEdges = (take 1 $ sortOn thrd edges') ++ (filter (not . connectedWithP x) edges)
            connectedWithP x (a,b,c)= any (\y -> (fst y == a || fst y == b) && (x == a || x == b)) (processed)
