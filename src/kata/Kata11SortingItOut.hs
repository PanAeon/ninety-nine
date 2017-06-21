module Kata11SortingItOut () where

import Data.Char
import qualified Data.HashMap.Lazy as HM

-- Sorting Balls

sortBalls :: Int -> [Int] -> [Int]
sortBalls x [] = [x]
sortBalls x (y:ys) | x <= y    = x : y : ys
                   | otherwise = y : sortBalls x  ys


-- Sorting characters

sortChars :: String -> String
sortChars = mapToList . foldr updateMap HM.empty


updateMap :: Char -> HM.HashMap Char Int -> HM.HashMap Char Int
updateMap ch = HM.insertWith (+) ch 1

mapToList :: HM.HashMap Char Int -> String
mapToList hm = ['a'..'z'] >>= \c -> replicate (HM.lookupDefault 0 c hm) c

prunePunctuation :: String -> String
prunePunctuation = map toLower . filter (\x -> isAscii x && isLetter x)
