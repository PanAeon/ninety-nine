{-# LANGUAGE ExistentialQuantification #-}

module HaskellForAll.BeautifulFolding() where

-- http://squing.blogspot.co.uk/2008/11/beautiful-folding.html


import Data.List(foldl')
import           Data.Strict.Tuple hiding (uncurry)

data Fold b c = forall a. F (a -> b -> a) a (a -> c)

both :: Fold b c -> Fold b c' -> Fold b (c, c')
both (F f x c) (F f' x' c') = F f'' x'' c''
  where
    f'' (y :!: y') z = f y z :!: f' y' z
    x'' = x :!: x'
    c'' (y :!: y') = (c y, c' y')
-- is like <*> for Applicative
both' :: Fold b c -> Fold b c' -> Fold b (c, c')
both' (F f x c) (F g y c') = F (comb f g) (x :!: y) (c *** c')
     where
         comb f g (a :!: a') b = f a b :!: g a' b -- like (first f) . (second g)
         (***) f g (x :!: y) = (f x, g y) -- like arrows

after :: Fold b c -> (c -> c') -> Fold b c'
after (F f x c) g = F f x (g . c)

-- why you don't like names, but choose to use stupidest one where
-- it is not needed?
bothWith :: (c -> c' -> d) -> Fold b c -> Fold b c' -> Fold b d
bothWith combiner f1 f2 = after (both f1 f2) (uncurry combiner)


cfoldl' :: Fold b c -> [b] -> c
cfoldl' (F f x c) = c . foldl' f x

sumF :: Num a => Fold a a
sumF = F (+) 0 id

productF :: Num a => Fold a a
productF = F (*) 1 id

lengthF :: Fold a Int
lengthF = F (const . (+1)) 0 id

meanF :: Fractional a => Fold a a
meanF = bothWith (/) sumF  (after lengthF fromIntegral)

mean :: Fractional a => [a] -> a
mean = cfoldl' meanF
