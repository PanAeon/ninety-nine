module Main where

import Control.Concurrent
import Control.Monad
import System.IO
import qualified Data.Map.Strict as Map
import Data.Map(Map(..))
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Vector(Vector(..))

data Philosopher = Philosopher { name :: String } deriving (Eq, Show, Ord)
type Fork = Int



philosophers = Philosopher <$> [ "A"
               , "B"
               , "C"
               , "D"
               , "E"
               ]


forkAssignment :: Map Philosopher [Fork]
forkAssignment = Map.fromList xs
  where
    n  = length philosophers
    xs = [ (p, [i, (i+1) `mod` n]) | (i,p) <- zip [0..] philosophers]


createForks :: IO (Vector (MVar Bool))
createForks = V.fromList <$> sequenceA [newMVar False | i <- [1..5]]

eat :: Philosopher -> IO ()
eat (Philosopher name) = do
  putStrLn $ name ++ " starts eating."
  threadDelay (10^3 * 312) -- FIXME: randomize the delay
  putStrLn $ name ++ " stops eating."

think :: Philosopher -> IO ()
think (Philosopher name) = do
  putStrLn $ name ++ " thinking..."
  threadDelay (10^3 * 713)

philosopher :: Vector (MVar Bool) -> Philosopher -> IO ()
philosopher forks phil = loop
  where
    loop = do
      think phil
      _ <- takeMVar f1 -- try to acquire "left" fork
      _ <- takeMVar f2 -- try to acquire "right" fork
      eat phil
      putMVar f1 False
      putMVar f2 False
      loop
    (f1:f2:[]) = (V.unsafeIndex forks) <$> (Map.!) forkAssignment  phil



main :: IO ()
main = do
  forks <- createForks
  void $ sequenceA [forkIO $ philosopher forks ph | ph <- philosophers ]
  threadDelay (10^6 * 4000)
