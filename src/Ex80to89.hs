module Ex80to89 (

) where

import Data.List(group, sort, findIndex, intersect, unfoldr, intersperse, nubBy, nub,
                    (\\), delete, inits, tails, sortBy, sortOn, permutations)
import Data.Maybe(fromJust, isJust)
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Monad.Loops(iterateWhile, unfoldM)
import Control.Applicative(liftA, Alternative, many)
import Control.Monad(ap, MonadPlus, mplus)
import qualified Control.Applicative as App
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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


-- Problem 85

-- (**) Graph isomorphism


equal' :: (Ord a, Eq a) => Graph a -> Graph a -> Bool
equal' (Graph n1 e1) (Graph n2 e2) = all (areAdjacent e1 e2) n1

areAdjacent e1 e2 x = getAdjacentNodes x e1 == getAdjacentNodes x e2
getAdjacentNodes :: (Ord a, Eq a) => a -> [(a,a)] -> Set.Set a
getAdjacentNodes x edges =  Set.fromList $ edges >>= (adjacentTo x)
adjacentTo x (a,b) = if x == a then [b] else if x == b then [a] else []
isAdjacentTo x (a,b) = x == a || x == b

getAdjacentNodesInOrder :: (Ord a, Eq a) => a -> [(a,a)] ->  [a]
getAdjacentNodesInOrder x edges =  edges >>= (adjacentTo x)


-- at the back




-- Problem 86
-- Color graph

nodeDegree :: (Eq a) => Graph a -> a -> Int
nodeDegree (Graph _ es) x = sum . ( map cnt ) . (concatMap (adjacentTo x)) . (filter (isAdjacentTo x)) $ es
   where
     cnt y | x == y    = 2
           | otherwise = 1

sortByDegreeDesc :: (Eq a) => Graph a -> [a]
sortByDegreeDesc g@(Graph xs es) = map fst $ sortOn (\x -> (-1)* (snd x)) $ zip xs $ map (nodeDegree g) xs



{-
Graph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
-}
colorGraph :: (Eq a, Ord a) => Graph a -> [(a, Int)]
colorGraph g@(Graph vs es) = color [1..] Map.empty v
       where
         v = sortByDegreeDesc g
         color _ _ [] = []
         color (c:cs) m (x:xs) = if Map.notMember x m then
                                   (x, c) : (color cs m'' xs) -- color all nodes (which are not colored) with c which do not collide
                                 else
                                   (x, fromJust $ Map.lookup x m) :  (color (c:cs) m xs)
           where
             isNothing Nothing = True
             isNothing _       = False
             m' = Map.insert x c m
             m'' = foldl colorIfPossible m' xs
             colorIfPossible m a = if  (isNothing $ Map.lookup a m) && isItPossible m a then Map.insert a c m else m
             isItPossible m a = all (\x -> (maybe 0 id $ Map.lookup x m)  /= c)  (getAdjacentNodes' a)
             getAdjacentNodes' x  =  Set.fromList $ es >>= (adjacentTo x)
             adjacentTo' x (a,b) = if x == a then [b] else if x == b then [a] else []


-- Problem 87
-- Depth-first order graph traversal (alternative solution)

depthfirst :: (Ord a, Eq a) => Graph a -> a -> [a]
depthfirst (Graph vertices edges) x = depth [x] Set.empty
    where
    --  depth::(Ord a, Eq a) =>  [a] -> Set.Set a -> [a]
      depth [] _ = []
      depth (x:xs) processed = x' ++ (depth xs' (Set.insert x processed))
          where
            x' = if (Set.member x processed) then [] else [x]
            xs' = (filter ((flip Set.notMember) processed)  (getAdjacentNodesInOrder x edges)) ++ xs


-- Problem 88
-- 10 Problem 88

-- (**) Connected components (alternative solution)

-- Write a predicate that splits a graph into its connected components.

connectedComponents :: (Eq a, Ord a) => Graph a -> [[a]]
connectedComponents g@(Graph vs es) = conn' $  vs
    where
      conn' [] = []
      conn' (x:xs) = cs : conn' (xs \\ cs)
        where
          cs = depthfirst g x

-- Problem 89
-- Bipartite graph
-- (I) assume that graph is connected
bipartite :: (Eq a, Ord a) => Graph a -> Bool
bipartite g@(Graph vs edges) = bip' [(head vs, 'u')] Set.empty Set.empty
     where
       getAdj x = getAdjacentNodes x edges
       adjToV x = map (\y -> (y, 'v')) (getAdjacentNodesInOrder x edges)
       adjToU x = map (\y -> (y, 'u')) (getAdjacentNodesInOrder x edges)
       bip' [] u v = True
       bip' ((x, 'u'):xs) u v | Set.member x u = bip' xs u v
                              | Set.member x v = False
                              | otherwise = bip' (adjToV x ++ xs) (Set.insert x u) v
       bip' ((x, 'v'):xs) u v | Set.member x v = bip' xs u v
                              | Set.member x u = False
                              | otherwise = bip' (adjToU x ++ xs) u (Set.insert x v)


----------------------- ISO -------------------------------------------------

graphG1 = Graph [1,2,3,4,5,6,7,8] [(1,5),(1,6),(1,7),(2,5),(2,6),(2,8),(3,5),(3,7),(3,8),(4,6),(4,7),(4,8)]
graphH1 = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,2),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]
graphH1' = Graph [1,2,3,4,5,6,7,8] [(1,2),(1,4),(1,5),(6,5),(6,7),(8,4),(8,5),(8,7),(3,2),(3,4),(3,7)]

-- FIXME: this is hard achieved victory, test it well
-- looks like adjacency list is better suitable for this job
-- TODO: Use list as monad here, should simplify a bit
-- I expect connected graph
iso :: (Ord a, Eq a) => Graph a -> Graph a -> Bool
iso ga@(Graph (va) ea) gb@(Graph (pivot:n2) eb) = any areIso' zs
  where
    labels0 = [2..]
    areIso [] _ _ _ _ = True
    areIso ((xa, xb):xs) labelsA labelsB nextLabels processedB = (getNEdgesA xa == getNEdgesB xb) &&
                                                                 (getLabel xa labelsA == getLabel xb labelsB) &&
                                                                 (Set.member xb processedB ||
                                                                 tailIsIsomorphic)
      where
        adjA = getAdjA xa
        adjB = getAdjB xb
        (labelsB', nextLabels') = labelNodes adjB labelsB nextLabels
        processedB' = Set.insert xb processedB
        foo = permutations adjA
        bar = fmap (\ys -> (ys, fst $ labelNodes ys labelsA nextLabels)) foo
        areIso'' (ys, labelsA') = areIso xs' labelsA' labelsB' nextLabels' processedB'
          where
            xs' = (zip ys adjB) ++ xs
        tailIsIsomorphic = any areIso'' bar
        -- now we have a list of labels for A ..., yeah, need to use []
      --  (labelsA', _) = labelNodes adjA

    getNEdgesA x = length $ getAdjacentNodes x ea
    getNEdgesB x = length $ getAdjacentNodes x eb
    getAdjA x = getAdjacentNodesInOrder x ea
    getAdjB x = getAdjacentNodesInOrder x eb
    getLabel x labels = fromJust $ Map.lookup x labels
    pivotNEdges = getNEdgesB pivot
    elligible = filter (\x -> getNEdgesA x  == pivotNEdges) va -- could have put whole graph instead, first step is to check
    ques0 = fmap (\x -> [(x, pivot)]) elligible
    labelssA0 =  fmap (\x -> Map.fromList [(x,1)]) elligible
    labelssB0 = replicate (length elligible) $ Map.fromList [(pivot, 1)]
    zs = zip3 ques0 labelssA0 labelssB0
    areIso' (q0, labelsA0, labelsB0) = areIso q0 labelsA0 labelsB0 labels0 Set.empty
    labelNodes [] labels nextLabels = (labels,nextLabels)
    labelNodes (x:xs) labels nextLabels = if (Map.member x labels) then
                                  labelNodes xs labels nextLabels
                               else
                                  labelNodes xs (Map.insert x (head nextLabels) labels) (tail nextLabels)

  --  padj = getAdjacentNodes pivot
  --  res = any (\x -> areIsomorphic [x]) elligible
  --  areIsomorphic [] = True -- check, this should be queue, not stack
  --  areIsomorphic queue = undefined
  --  res' = if length elligible == 0 then False else res
