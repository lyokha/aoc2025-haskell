module Main where

import Aoc01

main :: IO ()
main = do
    input01 <- lines <$> readFile "input01.txt"
    print $ aoc01 50 input01

