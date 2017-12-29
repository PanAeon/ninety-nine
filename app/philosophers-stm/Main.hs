module Main where

import Control.Concurrent
import Control.Monad
import Control.Concurrent.STM
import System.IO
import qualified Data.Map.Strict as Map
import Data.Map(Map(..))
import qualified Data.List as L
import qualified Data.Vector as V
import Data.Vector(Vector(..))

-- More, or less, but seems unnecessary complicated.. (maybe channels fit here poorly,
-- could have done the same with TVars )

data Philosopher = Philosopher { name :: String } deriving (Eq, Show, Ord)

philosophers = Philosopher <$> [ "A"
                               , "B"
                               , "C"
                               , "D"
                               , "E"
                               ]


eat :: Philosopher -> IO ()
eat (Philosopher name) = do
  putStrLn $ name ++ " starts eating."
  threadDelay (10^3 * 312) -- FIXME: randomize the delay
  putStrLn $ name ++ " stops eating."

think :: Philosopher -> IO ()
think (Philosopher name) = do
  putStrLn $ name ++ " thinking..."
  threadDelay (10^3 * 713)

{-

    1. For every pair of philosophers contending for a resource,
       create a fork and give it to the philosopher with the lower ID (n for agent Pn).
       Each fork can either be dirty or clean. Initially, all forks are dirty.
    2. When a philosopher wants to use a set of resources (i.e. eat),
       said philosopher must obtain the forks from their contending neighbors.
       For all such forks the philosopher does not have, they send a request message.
    3. When a philosopher with a fork receives a request message, they keep the fork if it is clean,
       but give it up when it is dirty. If the philosopher sends the fork over, they clean the fork before doing so.
    4. After a philosopher is done eating, all their forks become dirty.
       If another philosopher had previously requested one of the forks,
       the philosopher that has just finished eating cleans the fork and sends it.

-}
data Fork = Dirty | Clean deriving (Show, Eq)
data PhState = PhState { left        :: (Maybe Fork)
                       , right       :: (Maybe Fork)
                       }

philosopher' :: Philosopher -> TVar PhState -> TChan ASK_REQ -> TChan ASK_REQ -> TChan SENT_FORK ->
                               TChan SENT_FORK -> TChan SENT_FORK -> TChan SENT_FORK -> IO ()
philosopher' phil st lAskCh rAskCh lSendCh rSendCh recvL recvR = forever $ do
  think phil

  atomically $ do
    writeTChan lAskCh ASK_REQ
    writeTChan rAskCh ASK_REQ

  atomically $ readTChan recvL
  atomically $ readTChan recvR
  eat phil
  atomically $ do
    sendFork (\ph -> ph {left = Nothing }) st lSendCh
    sendFork (\ph -> ph {right = Nothing}) st rSendCh




sendFork :: (PhState -> PhState) -> TVar PhState -> TChan SENT_FORK -> STM ()
sendFork f st send = do
     phState <- readTVar st
     writeTVar st (f phState)
     writeTChan send SENT_FORK

respond :: (PhState -> PhState) -> (PhState -> Bool) -> TVar PhState -> TChan ASK_REQ -> TChan SENT_FORK -> IO ()
respond f g st recv send =  do
  atomically $ readTChan recv
  atomically $ do
    s <- readTVar st
    when (g s) $ sendFork f st send

createStates :: IO [TVar PhState]
createStates = sequenceA [
  newTVarIO (PhState fl fr) | i <- [0..4],
                                        let il = (i + 1) `mod` 5,
                                        let ir = (i - 1) `mod` 5,
                                        let fl = if (i < il)
                                                 then (Just Dirty)
                                                 else Nothing,
                                        let fr = if (i < ir)
                                                 then (Just Dirty)
                                                 else Nothing
         ]

-- askL askR sendL sendR recvL recvR
createChannels :: IO [(TChan ASK_REQ, TChan ASK_REQ, TChan SENT_FORK, TChan SENT_FORK)]
createChannels = sequenceA [ createCh | i <- [0..4]]
  where
    createCh = do
      askL <- newTChanIO
      askR <- newTChanIO
      sendL <- newTChanIO
      sendR  <- newTChanIO
      return (askL, askR, sendL, sendR)


-- philosopher' phil st lAskCh rAskCh lSendCh rSendCh recvL recvR
data ASK_REQ = ASK_REQ
data SENT_FORK = SENT_FORK

arrangeTable = do
  states <- createStates
  channels <- createChannels
  return [
           (philosopher' (philosophers !! i) (states !! i) askLOut askROut sendL sendR recvL recvR,
            respond (\ph -> ph { left = Nothing }) (\ph -> left ph == Just Dirty)  (states !! i) askLIn sendL,
            respond (\ph -> ph { right = Nothing }) (\ph -> right ph == Just Dirty) (states !! i) askRIn sendR
           )
           | i <- [0..4],
           let (askLOut, askROut, sendL, sendR) = channels !! i,
           let (_, askLIn, _, recvL) = channels !! ((i + 1) `mod` 5),  -- left
           let (askRIn, _ , recvR, _) = channels !! ((i - 1) `mod` 5) -- right
           ]


main :: IO ()
main = do
  arrangment <- arrangeTable
  void $ sequenceA $ do
    (ph, rl, rr) <- arrangment
    [forkIO ph, forkIO rl, forkIO rr]

  threadDelay (10^6 * 4000)

awaitBoth :: TChan a -> TChan a -> IO ()
awaitBoth ch1 ch2 = do
  res <- newTChanIO
  numCompleted <- newTVarIO 0
  _ <- forkIO (awaitSingle ch1 numCompleted res)
  _ <- forkIO (awaitSingle ch2 numCompleted res)
  void $ atomically $ readTChan res
  where
    awaitSingle ch ti res = atomically $ do
      _ <- readTChan ch
      i <- readTVar ti
      writeTVar ti (i+1)
      when (i == 1) $ writeTChan res ()
