module Aoc07 (readInput07, scanTopDown07, aoc07p2) where

import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Bifunctor
import GHC.IO (unsafePerformIO)
import System.Environment

readInput07 :: String -> (Int, [[Int]])
readInput07 =
    bimap (fromJust . elemIndex 'S') (map $ elemIndices '^')
    . fromJust . uncons
    . map snd
    . filter (even . fst)
    . zip [0 :: Int ..]
    . lines

scanTopDown07 :: Int -> [[Int]] -> [(Int, ([Int], [Int]))]
scanTopDown07 p =
    scanl (\(n, (pbeam, _)) ptach ->
              let is = intersect pbeam ptach
                  nbeam = map NE.head . NE.group $
                      foldr (\np a -> (np - 1) : (np + 1) : a) [] is
              in (n + length is, (sort $ pbeam `union` nbeam \\ is, is))
          ) (0, ([p], []))

memo ::(Int -> Int -> a) -> [[a]]
memo f = map (\x -> map (f x) [0..]) [0..]

vsS = map snd . uncurry scanTopDown07 . readInput07 $ unsafePerformIO (readFile "input07.txt")

go :: Int -> Int -> Integer
go n t
    | n == len - 1 = 2
    | otherwise = if (t - 1) `elem` (map fst vsS !! (n + 0))
                         then fastgo (n + 1) (t - 1)
                         else 0
                  + (if (t + 1) `elem` (map fst vsS !! (n + 0))
                          then fastgo (n + 1) (t + 1)
                          else 0)
    where len = length vsS

gostore :: [[Integer]]
gostore = memo go

fastgo :: Int -> Int -> Integer
fastgo x y = gostore !! x !! y

aoc07p2 :: [([Int], [Int])] -> Integer
aoc07p2 vs@(_ : (ps, [t0]) : _) = go 0 t0
aoc07p2 _ = undefined

