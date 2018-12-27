{-# LANGUAGE ExistentialQuantification #-}
module HaskellForAll.ComposableStreamingFolds() where


import           Control.Applicative
import           Data.List           (foldl')
import           Data.Monoid
import           Data.Strict.Tuple
import           Prelude             hiding (sum)

genericLength :: (Num a) => [a] -> a
genericLength = foldl' (const . (+1)) 0

sum :: (Num a) => [a] -> a
sum = foldl' (+) 0

stupidMean :: [Double] -> Double
stupidMean = go 0 0
  where
    go s l []     = s / fromIntegral l
    go s l (x:xs) = s `seq` l `seq`
                      go (s+x) (l+1) xs


data Fold a b = forall w. (Monoid w) => Fold {
    tally     :: a -> w
  , summarize :: w -> b
  }

fold :: Fold a b -> [a] -> b
fold (Fold t c) xs =
  c (foldl' mappend mempty (map t xs))


genericLength' :: (Num i) => Fold a i
genericLength' = Fold (\_ ->  Sum 1) (fromIntegral . getSum)

sum' :: (Num a) => Fold a a
sum' = Fold Sum getSum

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (aL :!: aR) <> (bL :!: bR) = (aL <> bL) :!: (aR <> bR)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = mempty :!: mempty
  mappend (aL :!: aR) (bL :!: bR) =
    mappend aL bL :!: mappend aR bR

instance Functor (Fold a) where
  fmap f (Fold t c) = Fold t (f . c)

-- Note that this uses strict Pairs from Data.Strict.Tuple to ensure that the combined
-- Fold still automatically runs in constant space
instance Applicative (Fold a) where
  pure x = Fold (const ()) (const x)
  (Fold t1 c1) <*> (Fold t c) = Fold (\x -> t x :!: t1 x) summarize
           where
             summarize (w :!: w1) = c1 w1 (c w)

average :: (Fractional a) => Fold a a
average = (/) <$> sum' <*> genericLength'


sumSq :: (Num a) => Fold a a
sumSq = Fold (\x -> Sum (x ^ 2)) getSum

std :: (Floating a) => Fold a a
std = (\ sq s l -> sqrt (sq / l - (s / l) ^ 2))
    <$> sumSq
    <*> sum'
    <*> genericLength'
    
