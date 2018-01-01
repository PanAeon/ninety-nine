{-# LANGUAGE RankNTypes #-}
module Polymorphism.Poly
    (
    ) where

import Control.Monad.ST
import Data.STRef
import Control.Monad.State
import System.Random
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

--v   = runST (newSTRef "abc") --  newSTRef :: a -> ST s (STRef s a)
--foo' = runST (readSTRef v)

-- all right, understood nothing, as usual

-- FROM https://www.stephanboyer.com/post/115/higher-rank-and-higher-kinded-types

-- The idea of higher-rank types is to make polymorphic functions first-class,
-- just like regular (monomorphic) functions.

-- let's try http://sleepomeno.github.io/blog/2014/02/12/Explaining-Haskell-RankNTypes-for-all/
-- lifehack let intLength :: [Int] -> Int; intLength = length

applyToTuple :: (forall a . [a] -> Int) -> ([b],[c]) -> (Int, Int)
applyToTuple f (a,b) = (f a, f b)

-- wow ((x.).)  trick !!!
coolSolution' = (((plusOne .) .) .). wasteFourArgs
 where
   wasteFourArgs = (\a b c d -> read a + read b)
   plusOne = (+1)

-- now let's see what's here https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html
data Player =
    Player {
      playerName :: String,
      playerPos  :: (Double, Double)
    }
    deriving (Eq, Ord, Show)

type GenAction m = forall a. (Random a) => m a

type GenActionR m = forall a. (Random a) => (a, a) -> m a


genRandom :: (RandomGen g) => GenAction (State g)
genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
    liftIO (putStrLn "Generating random player...")

    len <- genR (8, 12)
    name <- replicateM len (genR ('a', 'z'))
    x <- genR (-100, 100)
    y <- genR (-100, 100)

    liftIO (putStrLn "Done.")
    return (Player name (x, y))

 -- randomPlayer randomRIO >>= print
 -- FIXME: poorly understand the last part
-- Scott encoding, list fully described by unconsing
newtype ListS a =
    ListS {
      unconsS :: forall r. (a -> ListS a -> r) -> r -> r
    }

nilS :: ListS a
nilS = ListS (\co ni -> ni)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\co ni -> co x xs)

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni (ListS f) = f co ni

instance Functor ListS where
    fmap f =
        unconsS' (\x xs -> consS (f x) (fmap f xs))
                 nilS

-- Church encoding List is fully described by foldr

newtype ListC a =
    ListC {
      foldC :: forall r. (a -> r -> r) -> r -> r
    }
