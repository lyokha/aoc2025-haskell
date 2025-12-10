module Main where

import Control.Monad
import System.Environment

import Aoc01
import Aoc02
import Aoc03
import Aoc04
import Aoc05
import Aoc06
import Aoc07
import Aoc08
import Aoc09

main :: IO ()
main = do
    args <- getArgs
    when (null args || "1" `elem` args) $ do
        input <- lines <$> readFile "input01.txt"
        putStrLn ">>> Day 1"
        print $ aoc01 50 input
    when (null args || "2" `elem` args) $ do
        input <- ranges02 <$> readFile "input02.txt"
        putStrLn ">>> Day 2"
        print input
        let invalidIds1 = concatMap (aoc02 1) $ filterEven02 input
        print invalidIds1
        print $ sum invalidIds1
        let invalidIds2 = concatMap (aoc02 2) input
        print invalidIds2
        print $ sum invalidIds2
    when (null args || "3" `elem` args) $ do
        input <- lines <$> readFile "input03.txt"
        putStrLn ">>> Day 3"
        print $ aoc03p1 input
        print $ aoc03p2 input
    when (null args || "4" `elem` args) $ do
        input <- readFile "input04.txt"
        putStrLn ">>> Day 4"
        print $ aoc04p1 input
        print $ aoc04p2 input
    when (null args || "5" `elem` args) $ do
        input <- lines <$> readFile "input05.txt"
        putStrLn ">>> Day 5"
        let (ranges, values) = readInput05 input
            mergedRanges = mergeRanges05 ranges
        print mergedRanges
        print $ aoc05p1 mergedRanges values
        print $ aoc05p2 mergedRanges
    when (null args || "6" `elem` args) $ do
        input <- readFile "input06.txt"
        putStrLn ">>> Day 6"
        print $ aoc06p1 input
        print $ aoc06p2 input
    when (null args || "7" `elem` args) $ do
        input <- readFile "input07.txt"
        putStrLn ">>> Day 7"
        let ps@(pbeam, ptach) = readInput07 input
            scans = scanTopDown07 pbeam ptach
        print ps
        print scans
        print $ fst $ last scans
        print $ aoc07p2 $ map snd scans
    when (null args || "8" `elem` args) $ do
        input <- readInput08 . lines <$> readFile "input08.txt"
        print input
        print $ aoc08p1 1000 3 input
        print $ aoc08p2 input
    when (null args || "9" `elem` args) $ do
        input <- readInput09 . lines <$> readFile "input09.txt"
        print input
        print $ aoc09p1 input
        let points = aoc09p2 input
        print points
        print $ maximum points

