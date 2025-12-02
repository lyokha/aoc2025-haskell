module Main where

import Control.Monad
import System.Environment

import Aoc01
import Aoc02

main :: IO ()
main = do
    args <- getArgs
    when (null args || "1" `elem` args) $ do
        input01 <- lines <$> readFile "input01.txt"
        putStrLn ">>> Day 1"
        print (aoc01 50 input01)
    when (null args || "2" `elem` args) $ do
        input02 <- ranges02 <$> readFile "input02.txt"
        putStrLn ">>> Day 2"
        print input02
        let wrongIds = concatMap ranges02a input02
        print wrongIds
        print $ sum wrongIds

