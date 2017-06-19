module Labirinth () where



data Node a = DeadEnd a
              | Passage a (Node a)
              | Fork    a (Node a) (Node a)


get :: Node a -> a
get (DeadEnd a) = a
get (Passage a _) = a
get (Fork a _ _) = a


put :: a -> Node a -> Node a
put a (DeadEnd _) = DeadEnd a
put a (Passage _ n) = Passage a n
put a (Fork _ l r) = Fork a l r

type Zipper a = (Thread a, Node a)

data Branch a  = KeepStraightOn a
               | TurnLeft  a (Node a)
               | TurnRight a (Node a)
type Thread a  = [Branch a]



branchRight (Fork x l r) = TurnRight x l

turnRight :: Zipper a -> Maybe (Zipper a)
turnRight (ts, Fork a l r) = Just ((TurnRight a l):ts, r)
turnRight _ = Nothing

-- turnLeft :: Thread -> Thread
-- turnLeft ts = ts ++ [TurnLeft] -- notice ugly right append!!!
--
--
-- retrieve :: Thread -> Node a -> a
-- retrieve [] n = get n
-- retrieve (KeepStraightOn:ts) (Passage _ n) = retrieve ts n
-- retrieve (TurnRight:ts) (Fork _ _ r) = retrieve ts r
-- retrieve (TurnLeft:ts) (Fork _ l _) = retrieve ts l
--
-- update :: (a -> a) -> Thread -> Node a -> Node a
-- update f [] n = put (f (get n)) n
-- update f (KeepStraightOn:ts) (Passage _ n) = update f ts n
-- update f (TurnRight:ts) (Fork _ _ r) = update f ts r
-- update f (TurnLeft:ts) (Fork _ l _) = update f ts l
