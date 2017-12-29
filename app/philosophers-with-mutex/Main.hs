module Main where

import Control.Concurrent
import Control.Monad
import System.IO
import qualified Data.Map.Strict as Map
import Data.Map(Map(..))
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Vector(Vector(..))
import Data.IORef

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


createForks :: IO (Vector (IORef Bool))
createForks = V.fromList <$> sequenceA [newIORef False | i <- [1..5]]

eat :: Philosopher -> IO ()
eat (Philosopher name) = do
  putStrLn $ name ++ " starts eating."
  threadDelay (10^3 * 312) -- FIXME: randomize the delay
  putStrLn $ name ++ " stops eating."

think :: Philosopher -> IO ()
think (Philosopher name) = do
  putStrLn $ name ++ " thinking..."
  threadDelay (10^3 * 713)

philosopher :: MVar () ->  (Vector (IORef Bool))-> Philosopher -> IO ()
philosopher waiter forks phil = loop
  where
    loop = do
      think phil
      _ <- takeMVar waiter -- acquire waiter (here's small cheat, as this will queue other threads)
      f1 <- readIORef f1Ref
      f2 <- readIORef f2Ref
      if (f1 || f2)
      then do
        putMVar waiter ()
        loop
      else do
        writeIORef f1Ref True
        writeIORef f2Ref True
        putMVar waiter ()
        eat phil
        writeIORef f1Ref False
        writeIORef f2Ref False
        loop
    (f1Ref:f2Ref:[]) = (V.unsafeIndex forks) <$> (Map.!) forkAssignment  phil



main :: IO ()
main = do
  waiter <- newMVar ()
  forks <- createForks
  void $ sequenceA [forkIO $ philosopher waiter forks ph | ph <- philosophers ]
  threadDelay (10^6 * 4000)
