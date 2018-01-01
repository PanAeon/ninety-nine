{-# LANGUAGE BinaryLiterals #-}
module Comonads.SimpleCellularAutomata
    (
    ) where

import Data.Bits
import Data.List

-- from http://praisecurseandrecurse.blogspot.co.uk/2006/12/from-bits-to-cells-simple-cellular.html

{-
Rule numbers:

rule - depends on 3 bits of input - 2^3 = 8 total mappings
from 7 - to 0

111 110 101 100 011 010 001 000
 0   0   0   0   1   1   1   0

=> 00001110 (rule number)

-}

getNextBit :: Int -> (Bool, Bool, Bool) -> Bool
getNextBit ruleId (l,c,r) = testBit ruleId pos
  where
    b x = if x then 1 else 0
    pos = (4 * (b l) + 2 * (b c) + (b r))

rule_30 = getNextBit 30

-- TODO: wrap around?
nextGeneration :: Int -> [Bool] -> [Bool]
nextGeneration rule xs = fmap (getNextBit rule) zs
  where
    zs = zip3 (False:xs) xs (drop 1 xs ++ [False])

-- unfoldr :: ([] -> Maybe (a, [])) -> [] -> [a]
runAutomata :: Int -> Int -> Int -> [[Bool]]
runAutomata rule width nSteps = take nSteps $ iterate (nextGeneration rule) initial
  where
    initial = let
                zeros = take (width `div` 2) (repeat False)
              in zeros ++ [True] ++ zeros

-- very inefficient :( but this is not the goal
visualize :: [[Bool]] -> String
visualize = unlines . fmap (fmap toCh)
   where
     toCh b = if b then '#' else ' '

-- putStrLn $ visualize $ runAutomata 82 63 32

{-
                               #
                              # #
                             #   #
                            # # # #
                           #       #
                          # #     # #
                         #   #   #   #
                        # # # # # # # #
                       #               #
                      # #             # #
                     #   #           #   #
                    # # # #         # # # #
                   #       #       #       #
                  # #     # #     # #     # #
                 #   #   #   #   #   #   #   #
                # # # # # # # # # # # # # # # #
               #                               #
              # #                             # #
             #   #                           #   #
            # # # #                         # # # #
           #       #                       #       #
          # #     # #                     # #     # #
         #   #   #   #                   #   #   #   #
        # # # # # # # #                 # # # # # # # #
       #               #               #               #
      # #             # #             # #             # #
     #   #           #   #           #   #           #   #
    # # # #         # # # #         # # # #         # # # #
   #       #       #       #       #       #       #       #
  # #     # #     # #     # #     # #     # #     # #     # #
 #   #   #   #   #   #   #   #   #   #   #   #   #   #   #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

-}
