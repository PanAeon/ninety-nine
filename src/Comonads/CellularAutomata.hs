module Comonads.CellularAutomata
    (
    ) where

-- from http://blog.sigfpe.com/2006/12/evaluating-cellular-automata-is.html

-- and now the fun starts


data U x = U [x] x [x]


right (U l c (r:rs)) = U (c:l) r rs
left  (U (l:ls) c r) = U ls l (c:r)

instance Functor U where
  fmap f (U l c r) = U (map f l) (f c) (map f r)

class Functor w => Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)

instance Comonad U where
  coreturn (U _ c _) = c
  cojoin x = U (tail $ iterate left x) x (tail $ iterate right x)

rule (U (a:_) b (c:_)) = not (a && b && not c || (a==b))

-- FIXME: too hard below, needs rethinking, looks like more
-- or less understand, but ...

shift i u = (iterate (if i<0 then left else right) u) !! abs i

toList i j u = take (j-i) $ half $ shift i u where
     half (U _ b c) = [b] ++ c

test = let u = U (repeat False) True (repeat False)
       in putStr $
          unlines $
          take 32 $
          map (map (\x -> if x then '#' else ' ') . toList (-32) 32) $
          iterate (=>> rule) u
