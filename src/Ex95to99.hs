module Ex90to94 (

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
