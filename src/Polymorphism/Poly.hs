{-# LANGUAGE RankNTypes #-}
module Polymorphism.Poly
    (
    ) where

import Control.Monad.ST

-- main article: "On understanding types, data abstractions and ploymorphism"
-- http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf
-- good explanation: https://en.wikibooks.org/wiki/Haskell/Polymorphism

length' :: forall a. [a] -> Int
length' = foldr (const (+1)) 0

foo :: (forall a. a -> a) -> (Char,Bool)
foo f = (f 'c', f True)

bar :: forall a. ((a -> a) -> (Char, Bool))
bar f = undefined -- doesn't compile, because a is fixed (f 'c', f True)

runST' :: (forall s. ST s a) -> a
runST' = runST
