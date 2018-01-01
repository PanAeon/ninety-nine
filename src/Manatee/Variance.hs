{-# LANGUAGE DeriveFunctor #-}

module Manatee.Variance
    (
    ) where

{- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
So,
consumers - contravariant
producers - covariant

example -- Function1[-A,+B]

all, right, so back to pandas, we have

Food <: Vegs <: Bamboo

Producer[Food] <: Producer[Vegs] <: Producer[Bamboo],
because you can substitute producer of food with producer of vegs

but
Consumer[Bamboo] <: Consumer[Vegs] <: Consumer[Food],
you can subst. consumer of bamboo with consumber of food, not vise verse.

goes from Liskov substitution principle


Be conservative in what you send; be liberal in what you accept.
  which means you can return only subtype, but should be able to handle supertype? (probably when overloading)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -}

-- now https://www.fpcomplete.com/blog/2016/11/covariance-contravariance

import Data.Functor.Contravariant
import System.Random
import System.IO
import Control.Monad.IO.Class

showInt' :: Int -> String
showInt' = show

floorInt :: Double -> Int
floorInt = floor

maybeInt :: Maybe Int
maybeInt = Just 5

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe = fmap

maybeString :: Maybe String
maybeString = fmapMaybe showInt' maybeInt

newtype MakeString a = MakeString { makeString :: a -> String }

instance Contravariant MakeString where
    contramap f (MakeString g) = MakeString (g . f)

showInt :: MakeString Int
showInt = MakeString show

plus3ShowInt :: MakeString Int
plus3ShowInt = contramap (+ 3) showInt

mapMakeString :: (b -> a) -> MakeString a -> MakeString b
mapMakeString f (MakeString g) = MakeString (g . f)

greaterThen3 :: Predicate Int
greaterThen3 = Predicate (>3)

lengthGreaterThen3 :: Predicate [a]
lengthGreaterThen3 = contramap length greaterThen3
                     -- (>3) . length

english :: Int -> String
english 1 = "one"
english 2 = "two"
english 3 = "three"
english 4 = "four"
english 5 = "five"
english 6 = "six"
english 7 = "seven"
english 8 = "eight"
english 9 = "nine"
english 10 = "ten"

englishGreaterThen3 :: Predicate Int
englishGreaterThen3 = contramap english lengthGreaterThen3

-- Bifunctor and Profunctor


class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

class Profunctor p where
    dimap :: (b -> a) -> (c -> d) -> p a c -> p b d


instance Bifunctor Either where
    bimap f _ (Left x) = Left (f x)
    bimap _ f (Right x) = Right (f x)
instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)

instance Profunctor (->) where -- functions
    dimap f g h = g . h . f


-- Bivariant and invariant

-- bivariant functor doesn't exist
data Phantom a = Phantom
instance Functor Phantom where
    fmap _ Phantom = Phantom
instance Contravariant Phantom where
    contramap _ Phantom = Phantom

{- Invariance will occur if:
   - A type parameter is used multiple times in a data structure,
     both positively and negatively, e.g.:

     data ToFrom a = ToFrom (a -> Int) (Int -> a)

   - A type parameter is used in type which is itself invariant
     in the parameter, e.g.:

     newtype ToFromWrapper a = ToFromWrapper (ToFrom a)

   - In special types like references, e.g.:
     data IORef a -- a is invariant
     newtype RefWrapper a = RefWrapper (IORef a) -- also invariant
-}

-- Ex1.
data ToFrom a = ToFrom (a -> Int) (Int -> a)

-- instance Functor ToFrom where
--   fmap h (ToFrom f g) = ToFrom _ (h . g)


---- Positive and negative position ---

data WithInt a = WithInt (Int -> a)
data MakeInt a = MakeInt (a -> Int)

--  Positive position: the type variable is the result/output/range/codomain of the function
--  Negative position: the type variable is the argument/input/domain of the function


type Callback a = a -> IO ()

--newtype CallbackRunner a = CallbackRunner (Callback a -> IO ())
newtype CallbackRunner a =
  CallbackRunner { runCallback :: (a -> IO ()) -> IO ()
                 }

instance Functor CallbackRunner where
  fmap f (CallbackRunner g) =
      CallbackRunner (\h -> g (h . f))

supplyRandom :: CallbackRunner Int
supplyRandom = CallbackRunner $ \callback -> do
    int <- randomRIO (1, 10)
    callback int

-- Lifting IO to MonadIO

openFileLifted :: MonadIO m => FilePath -> IOMode -> m Handle
openFileLifted fp mode = liftIO (openFile fp mode)


-- can't be done
-- withFileLifted :: MonadIO m => FilePath -> IOMode -> (Handle -> m a) -> m a
-- acquire, release, in-between
bracket:: IO a -> (a -> IO b)
               -> (a -> IO c)
               -> IO c
bracket = undefined

-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

withFile' :: FilePath -> IOMode -> (Handle -> IO c) -> IO c
withFile' name mode = bracket (openFile name mode) hClose

-- ???
withFileLifted :: MonadIO m => FilePath -> IOMode -> (Handle -> IO a) -> m a
withFileLifted name mode handle = liftIO (withFile' name mode handle)


 
