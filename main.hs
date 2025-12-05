module Main where

import Control.Monad
import System.Environment

import Aoc01
import Aoc02
import Aoc03
import Aoc04
import Aoc05

main :: IO ()
main = do
    args <- getArgs
    when (null args || "1" `elem` args) $ do
        input01 <- lines <$> readFile "input01.txt"
        putStrLn ">>> Day 1"
        print $ aoc01 50 input01
    when (null args || "2" `elem` args) $ do
        input02 <- ranges02 <$> readFile "input02.txt"
        putStrLn ">>> Day 2"
        print input02
        let invalidIds1 = concatMap (aoc02 1) $ filterEven02 input02
        print invalidIds1
        print $ sum invalidIds1
        let invalidIds2 = concatMap (aoc02 2) input02
        print invalidIds2
        print $ sum invalidIds2
    when (null args || "3" `elem` args) $ do
        input03 <- lines <$> readFile "input03.txt"
        putStrLn ">>> Day 3"
        print $ aoc03p1 input03
        print $ aoc03p2 input03
    when (null args || "4" `elem` args) $ do
        input04 <- readFile "input04.txt"
        putStrLn ">>> Day 4"
        print $ aoc04p1 input04
        print $ aoc04p2 input04
    when (null args || "5" `elem` args) $ do
        input05 <- lines <$> readFile "input05.txt"
        putStrLn ">>> Day 5"
        let (ranges, values) = readInput05 input05
            mergedRanges = mergeRanges05 ranges
        print mergedRanges
        print $ aoc05p1 mergedRanges values
        print $ aoc05p2 mergedRanges

