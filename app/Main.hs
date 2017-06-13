module Main where

import Lib
import Continuations

foobar = calculateLength [1,2,3,4,5]

main :: IO ()
main = print $ myButLast [1,2,3,4,5]
