module Main where

import Control.Monad
import System.Environment

import Aoc01

main :: IO ()
main = do
    args <- getArgs
    when (null args || "1" `elem` args) $ do
        input01 <- lines <$> readFile "input01.txt"
        putStrLn ">>> Day 1" >> print (aoc01 50 input01)

