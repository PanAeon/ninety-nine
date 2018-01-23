{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Distributions.Part1
    (
    ) where

-- http://web.engr.oregonstate.edu/%7Eerwig/pfp/

-- import qualified Numeric.Probability.Distribution as Dist
-- import Numeric.Probability.Distribution ((??), )
-- import Control.Monad (liftM2, replicateM)
import Control.Monad.Trans.Class
import Control.Monad
import Data.List( foldl', group, sort)
import System.Random
import Control.Monad.Trans.Maybe
import qualified Data.Map as M
import Data.Maybe(catMaybes)


newtype Prob = P Float
  deriving (Eq, Ord, Num, Fractional)

instance Show Prob where
  show (P p) = show intPart ++ "." ++ show fracPart ++ "%"
    where digits = round (1000 * p)
          intPart = digits `div` 10
          fracPart = digits `mod` 10

data Perhaps a = Perhaps a Prob
  deriving (Show)

neverHappens (Perhaps _ 0) = True
neverHappens _             = False

-- never = Perhaps undefined 0

instance Functor Perhaps where
  fmap f (Perhaps x p) = Perhaps (f x) p


instance Applicative Perhaps where
  pure x = Perhaps x 1
  Perhaps f pf <*> Perhaps x px = Perhaps (f x) (pf * px)

instance Monad Perhaps where
  return x = Perhaps x 1
  ph >>= f  | neverHappens ph  = never
            | otherwise        = Perhaps x (p1 * p2)
    where (Perhaps (Perhaps x p1) p2) = fmap f ph

class Monad m => MonadPerhaps m where
  perhaps :: a -> Prob -> m a
  never :: m a

instance MonadPerhaps Perhaps where
  never = Perhaps undefined 0
  perhaps = Perhaps


newtype PerhapsT m a = PerhapsT { runPerhapsT :: m (Perhaps a) }

instance MonadTrans PerhapsT where
  lift x = PerhapsT (liftM return x)

instance Monad m => Functor (PerhapsT m) where
  fmap = liftM

instance Monad m => Applicative (PerhapsT m) where
  pure = lift . return
  f <*> m  = PerhapsT bound
    where
      bound = do
              (Perhaps x1 p1) <- runPerhapsT m
              (Perhaps f' p2)  <- runPerhapsT f
              return $ Perhaps (f' x1) (p1 * p2)

instance Monad m => Monad (PerhapsT m) where
  return = lift . return
  m >>= f = PerhapsT bound
    where bound = do
            ph <- runPerhapsT m
            case ph of
              (Perhaps x1 p1)  | p1 == 0    -> return never
                               | otherwise  -> do
                (Perhaps x2 p2) <- runPerhapsT (f x1)
                return (Perhaps x2 (p1 * p2))

type FDist = PerhapsT ([])

instance Dist FDist where
  weighted [] = error "Empty distribution"
  weighted xws = PerhapsT (map weight xws)
    where weight (x,w) = Perhaps x (P (w / sum))
          sum = foldl' (+) 0 (map snd xws)

exact :: FDist a -> [Perhaps a]
exact = runPerhapsT


-- type Dist = PerhapsT ([])

-- uniform = weighted . map (\x -> (x, 1))

-- weighted :: [(a, Float)] -> Dist a
-- weighted [] =
--   error "Empty probability distributuion"
-- weighted xws = PerhapsT (map weight xws)
--   where weight (x,w) = Perhaps x (P (w / sum))
--         sum = foldl' (\w1 (_,w2) -> w1+w2) 0 xws


die :: Dist d => d Int
die = uniform [1..6]

type Weight = Float

class (Functor d, Monad d) => Dist d where
  weighted :: [(a, Weight)] -> d a


weight2F :: Weight -> Float
weight2F x = x

uniform :: Dist d => [a] -> d a
uniform = weighted . map (\x -> (x, 1))

data Child = Girl | Boy
  deriving (Show, Eq, Ord)

child :: Dist d => d Child
child = uniform [Girl, Boy]

family :: Dist d => d [Child]
family = do
  child1 <- child
  child2 <- child
  return [child1, child2]

newtype Rand a = Rand { runRand :: IO a }

randomFloat :: Rand Float
randomFloat = Rand (getStdRandom (random))

instance Functor Rand where
  fmap = liftM

instance Applicative Rand where
  pure = return
  mf <*> ma  = do
               f <- mf
               a <- ma
               return (f a)


instance Monad Rand where
  return x = Rand (return x)
  r >>= f = Rand (do x <- runRand r
                     runRand (f x))

instance Dist Rand where
  weighted = liftF . weighted

liftF :: FDist a -> Rand a
liftF fdist = do
  n <- randomFloat
  pick (P n) (runPerhapsT fdist)

pick :: Monad m => Prob -> [Perhaps a] -> m a
pick _ [] = fail "No values to pick from"
pick n ((Perhaps x p):ps)
  | n <= p    = return x
  | otherwise = pick (n-p) ps

sample :: Rand a -> Int -> Rand [a]
sample r n = sequence (replicate n r)

sampleIO r n = runRand (sample r n)

histogram :: Ord a => [a] -> [Int]
histogram = map length . group . sort

{-


    A very senior Microsoft developer who moved to Google told me
    that Google works and thinks at a higher level of abstraction than Microsoft.
    "Google uses Bayesian filtering the way Microsoft uses the if statement," he said.
        -Joel Spolsky

-}

data Test = Pos | Neg
  deriving (Show, Eq)

data HeroinStatus = User | Clean
  deriving (Show, Eq)


drugTest1 :: Dist d => d (HeroinStatus, Test)
drugTest1 = do
  heroinStatus <- percentUser 0.1
  testResult <-
    if heroinStatus == User
      then percentPos 99
      else percentPos 1
  return (heroinStatus, testResult)

-- Some handy distributions.
percentUser p = percent p User Clean
percentPos p = percent p Pos Neg

-- A weighted distribution with two elements.
percent p x1 x2 =
  weighted [(x1, p), (x2, 100-p)]

drugTest2 :: Dist d => d (Maybe HeroinStatus)
drugTest2 = do
  (heroinStatus, testResult) <- drugTest1
  return (if testResult == Pos
            then Just heroinStatus
            else Nothing)

value (Perhaps x _) = x
prob (Perhaps _ p) = p

catMaybes' :: [Perhaps (Maybe a)] -> [Perhaps a]
catMaybes' [] = []
catMaybes' (Perhaps Nothing _ : xs) =
  catMaybes' xs
catMaybes' (Perhaps (Just x) p : xs) =
  Perhaps x p : catMaybes' xs

onlyJust :: FDist (Maybe a) -> FDist a
onlyJust dist
    | total > 0 = PerhapsT (map adjust filtered)
    | otherwise = PerhapsT []
  where filtered = catMaybes' (runPerhapsT dist)
        total = sum (map prob filtered)
        adjust (Perhaps x p) =
          Perhaps x (p / total)

type FDist' = MaybeT FDist
-- type FDist' = MaybeT (PerhapsT [])


-- Monads are Functors, no matter what
-- Haskell thinks.
-- instance Functor FDist' where
--   fmap = liftM

instance Dist FDist' where
  weighted xws = lift (weighted xws)


bayes :: FDist' a -> [Perhaps a]
bayes = exact . onlyJust . runMaybeT

condition :: Bool -> FDist' ()
condition = MaybeT . return . toMaybe
  where toMaybe True  = Just ()
        toMaybe False = Nothing

drugTest3 :: FDist' HeroinStatus ->
             FDist' HeroinStatus
drugTest3 prior = do
  heroinStatus <- prior
  testResult <-
    if heroinStatus == User
      then percentPos 99
      else percentPos 1
  -- As easy as an 'if' statement:
  condition (testResult == Pos)
  return heroinStatus

data MsgType = Spam | Ham
  deriving (Show, Eq, Enum, Bounded)


hasWord :: String -> FDist' MsgType ->
           FDist' MsgType
hasWord word prior = do
  msgType <- prior
  wordPresent <-
    wordPresentDist msgType word
  condition wordPresent
  return msgType

hasWords []     prior = prior
hasWords (w:ws) prior = do
  hasWord w (hasWords ws prior)

-- [Spam count, ham count]
msgCounts = [102, 57]

-- Number of spams and hams containing
-- each word.
wordCountTable =
  M.fromList [("free", [57, 6]),
              -- Lots of words...
              ("bayes", [1, 10]),
              ("monad", [0, 22])]

-- This is basically a Haskell
-- version of 'ys[x]'.
entryFor :: Enum a => a -> [b] -> b
entryFor x ys = ys !! fromEnum x

msgTypePrior :: Dist d => d MsgType
msgTypePrior =
  weighted (zip [Spam,Ham] msgCounts)


wordPresentDist msgType word =
    boolDist ( P (( n)/total))
  where wordCounts = findWordCounts word
        n     = entryFor msgType wordCounts
        total = entryFor msgType msgCounts

boolDist :: Prob -> FDist' Bool
boolDist (P p) =
    weighted [(True, p), (False,   1-p)]

findWordCounts word =
  M.findWithDefault [0,0] word wordCountTable

uniformAll :: (Dist d,Enum a,Bounded a) => d a
uniformAll = uniform allValues

allValues :: (Enum a, Bounded a) => [a]
allValues = enumFromTo minBound maxBound

characteristic f = f uniformAll


score f =
  distance (characteristic f) uniformAll

-- Euclidean distance, minus the final
-- square root (which makes no difference
-- and wastes a lot of cycles).
distance :: (Eq a, Enum a, Bounded a) =>
            FDist' a -> FDist' a -> Double
distance dist1 dist2 =
    sum (map (^2) (zipWith (-) ps1 ps2))
  where ps1 = vectorFromDist dist1
        ps2 = vectorFromDist dist2


doubleFromProb :: Prob -> Double
doubleFromProb (P p) = realToFrac p

vectorFromDist dist =
  map doubleFromProb (probsFromDist dist)

probsFromDist dist =
    map (\x -> (sumProbs . matching x) (bayes dist))
        allValues
  where matching x = filter ((==x) . perhapsValue)
        sumProbs = sum . map perhapsProb


perhapsValue :: Perhaps a -> a
perhapsValue (Perhaps x _ ) = x

perhapsProb :: Perhaps a -> Prob
perhapsProb (Perhaps _ p ) = p


adjustedProbsFromDist dist =
  adjustMinimums (probsFromDist dist)

adjustMinimums xs = map (/ total) adjusted
  where adjusted = map (max 0.01) xs
        total = sum adjusted

-- Convert f to a probability vector.
classifierProbs f =
   adjustedProbsFromDist (characteristic f)

-- Apply a probability vector to a prior
-- distribution.
applyProbs probs prior = do
  msgType <- prior
  applyProb (entryFor msgType probs)
  return msgType

applyProb :: Prob -> FDist' ()
applyProb p = do
  b <- boolDist p
  condition b

data Classifier = Classifier Double [Prob]
  deriving Show

classifier f =
  Classifier (score f) (classifierProbs f)

applyClassifier (Classifier _ probs) =
  applyProbs probs

instance Eq Classifier where
  (Classifier s1 _) == (Classifier s2 _) =
      s1 == s2

instance Ord Classifier where
  compare (Classifier s1 _)
          (Classifier s2 _) =
    compare s2 s1


classifiers :: M.Map String Classifier
classifiers =
    M.mapWithKey toClassifier wordCountTable
  where toClassifier w _ =
          classifier (hasWord w)

findClassifier :: String -> Maybe Classifier
findClassifier w = M.lookup w classifiers

findClassifiers n ws =
    take n (sort classifiers)
  where classifiers =
          catMaybes (map findClassifier ws)

hasWords' ws prior =
  foldr applyClassifier
        prior
        (findClassifiers 15 ws)
