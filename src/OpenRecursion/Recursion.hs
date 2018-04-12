{-# LANGUAGE RankNTypes #-}
module OpenRecursion.Recursion() where

-- from http://www.well-typed.com/blog/2018/03/oop-in-haskell/
import           Control.Lens
import           Control.Lens.Setter
import           Control.Lens.Getter


fac :: (Int -> Int) -> Int -> Int
fac r 1 = 1
fac r n = n * r (n - 1)

fix :: (a -> a) -> a
fix f = f (fix f)

fac' = fix fac

skim:: (Int -> Int) -> Int -> Int
skim r 1 = 1
skim r n = (fac r n) - 1

data Counter = Counter {
      tick    :: Counter
    , tock    :: Counter
    , display :: String
    }

mkCounter :: Lens' st Int -> (st -> Counter) -> (st -> Counter)
mkCounter l self st = Counter {
     tick    = self (over l (+ 1) st)
   , tock    = self (over l (+ 1) st)
   , display = show (view l st)
}

-- twice :: (Int -> Counter) -> (Int -> Counter)
-- twice self n = (mkCounter self n) {
--                  display = n * 2
--                }

-- ticktock :: ((Int, Int) -> Counter) -> (Int, Int) -> Counter
-- ticktock self (n, m) = mkCounter (self . _reconstruct) n

ticktock :: Lens' st (Int, Int)
         -> (st -> Counter) -> (st -> Counter)
ticktock l self st = (mkCounter (l . _1) self st) {
      tock    = self (over (l . _2) (+ 1) st)
    , display = show (view l st)
    }
