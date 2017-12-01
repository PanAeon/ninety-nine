module Ex95to99 (

) where

import Data.List(group, sort, findIndex, intersect, unfoldr, intersperse, nubBy, nub,
                    (\\), delete, inits, tails, sortBy, sortOn, permutations,
                    intercalate)
import Data.Maybe(fromJust, isJust)
import Data.Traversable(traverse)
import qualified Data.Foldable as Fldbl
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as St
import Control.Monad.Loops(iterateWhile, unfoldM)
import Control.Applicative(liftA, Alternative, many)
import Control.Monad(ap, MonadPlus, mplus)
import qualified Control.Applicative as App
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
-- Problem 95

-- (**) English number words


fullWords :: Int -> String
fullWords n = intercalate "-" wordz
  where
    digits = reverse . unfoldr (\x -> case x of
                               0 -> Nothing
                               n -> Just (x `mod` 10, x `div` 10)
                     )
    wordz = map (wrdz V.! ) $ digits n
    wrdz = V.fromList ["zero", "one", "two", "three", "four",
                  "five", "six", "seven", "eight", "nine"]

        -- .  .  4 | 8  .  . | .  1  7	     9  3  4 | 8  2  5 | 6  1  7
        --         |         |                          |         |
        -- 6  7  . | 9  .  . | .  .  .	     6  7  2 | 9  1  4 | 8  5  3
        --         |         |                          |         |
        -- 5  .  8 | .  3  . | .  .  4          5  1  8 | 6  3  7 | 9  2  4
        -- --------+---------+--------          --------+---------+--------
        -- 3  .  . | 7  4  . | 1  .  .          3  2  5 | 7  4  8 | 1  6  9
        --         |         |                          |         |
        -- .  6  9 | .  .  . | 7  8  .          4  6  9 | 1  5  3 | 7  8  2
        --         |         |                          |         |
        -- .  .  1 | .  6  9 | .  .  5          7  8  1 | 2  6  9 | 4  3  5
        -- --------+---------+--------          --------+---------+--------
        -- 1  .  . | .  8  . | 3  .  6	     1  9  7 | 5  8  2 | 3  4  6
        --         |         |                          |         |
        -- .  .  . | .  .  6 | .  9  1	     8  5  3 | 4  7  6 | 2  9  1
        --         |         |                          |         |
        -- 2  4  . | .  .  1 | 5  .  .          2  4  6 | 3  9  1 | 5  7  8




-- Sudoku

type PlainSudoku = [Int]

-- rows, cols, boxes
data ExplodedSudoku = ExplodedSudoku [IntSet] [IntSet] [IntSet]

-- TODO: put two spaces between middle digits
-- TODO: parse output
exampleSudoku :: [Int]
exampleSudoku = [ 0, 0, 4,  8, 0, 0,  0, 1, 7,
                  6, 7, 0,  9, 0, 0,  0, 0, 0,
                  5, 0, 8,  0, 3, 0,  0, 0, 4,

                  3, 0, 0,  7, 4, 0,  1, 0, 0,
                  0, 6, 9,  0, 0, 0,  7, 8, 0,
                  0, 0, 1,  0, 6, 9,  0, 0, 5,

                  1, 0, 0,  0, 8, 0,  3, 0, 6,
                  0, 0, 0,  0, 0, 6,  0, 9, 1,
                  2, 4, 0,  0, 0, 1,  5, 0, 0
                  ]

-- TODO: all right encode later, try to solve with plain lists

explodeSudoku :: PlainSudoku -> ExplodedSudoku
explodeSudoku xs = ExplodedSudoku rows cols boxes
  where
    (rows, cols, boxes) = foldl f  ([], [], []) (zip xs [0..])
    f (rs, cs, bs) (x, i) = undefined
    row i = i div 9
    col i = i mod 9
    box i = undefined



printSudoku :: PlainSudoku -> String
printSudoku xs = (zip xs [1..]) >>= foo
  where
    foo (a,i) | i /= 81 && i `mod` 27 == 0 = (printDigit a) ++ "\n--------+---------+--------\n"
              | i `mod` 9 == 0 = (printDigit a) ++ "\n"
              | i `mod` 3 == 0 = (printDigit a) ++ " | "
              | otherwise      = (printDigit a) ++ "  "
    printDigit i | i == 0 = "."
                 | otherwise = (show i)


solveSudoku :: PlainSudoku -> PlainSudoku
solveSudoku = undefined
