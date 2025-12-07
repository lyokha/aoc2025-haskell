module Aoc07 (readInput07, scanTopDown07, aoc07p2) where

import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Bifunctor

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

memo2d :: (Int -> Int -> a) -> [[a]]
memo2d f = map (\x -> map (f x) [0..]) [0..]

aoc07p2 :: [([Int], [Int])] -> Integer
-- expect the upper tachyon at line 2 (modulo lines free of tachyons)
aoc07p2 vs@(_ : (_, [t0]) : _) = go 1 t0
    where go n t
              | n == len = 0
              | otherwise = let (nl, nr) = (snd $ ptachNext (n + 1) $ t - 1
                                           ,snd $ ptachNext (n + 1) $ t + 1
                                           )
                            in case nl of
                                   Just nl' -> fastgo nl' $ t - 1
                                   Nothing -> 1
                               +
                               case nr of
                                    Just nr' -> fastgo nr' $ t + 1
                                    Nothing -> 1
          ptach = map snd vs
          len = length ptach
          ptachNext nk nn =
              foldl (\(k', n') ptach' ->
                        case n' of
                            Nothing -> if nn `elem` ptach'
                                           then (k', Just k')
                                           else (k' + 1, Nothing)
                            n'' -> (k' + 1, n'')
                    ) (nk, Nothing) $ drop nk ptach
          gostore = memo2d go
          fastgo x y = gostore !! x !! y
aoc07p2 _ = undefined

