{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE Rank2Types          #-}

module Queenslogic.MyQueensLogic where

import           Control.Monad.Logic
import           Data.List           (intersect)

-- FIXME: write one from scratch in a month!
instance Show a => Show (Logic a) where
    show l = "Logic> " ++ show $ observeAll l

-- FIXME: what is choices?
choices :: MonadLogic m => [a] -> m a
choices = msum . map return

-- K is for Kleisli
-- FIXME: why do we need Kleisli here ?
type K m a = Monad m => (m a -> (a -> m a) -> m a)

data Diagonal = Row Int
              | Col Int
              | Backslash Int
              | Forwardslash Int
              deriving (Eq, Show)


type Square = (Int,Int)
type Queens = [Int] -- a configuration of nonattacking queens on the board
type Q = Queens

diags :: Square -> [Diagonal]
diags (x,y) = [
                Row x
              , Col y
              , Backslash (x + y)
              , Forwardslash (x - y)
              ]

isSafeToInsert :: Square -> Q -> Bool
isSafeToInsert s qs = null $ diags s `intersect` underThreat 
   where
      qs' = zip [1..length qs]  qs
      underThreat = qs' >>= diags

allWaysToAdd :: MonadLogic m => Int -> Q -> m Q
allWaysToAdd n qs = [ qs ++ [j] | j <- choices [1..n], (i,j) `isSafeToInsert` qs]
   where
     i = length qs + 1

queens :: MonadLogic m => Int -> K m Q -> m Q
queens n (>>~) = foldl (>>~) (return mzero) (replicate n (allWaysToAdd n))
