module Aoc08 (readInput08, aoc08p1, aoc08p2) where

import Data.List
import Data.Function
import Data.Bifunctor
import Data.DisjointSet qualified as DS

type Point = (Int, (Int, Int, Int))

readInput08 :: [String] -> [Point]
readInput08 =
    zipWith (\i -> (\(x, y, z) -> (i, (read x, read y, read z)))
                   . snd
                   . foldr (\v (n, p@(x, y, z)) ->
                               if v == ','
                                   then (n + 1, p)
                               else (n, case n of
                                            0 -> (x, y, v : z)
                                            1 -> (x, v : y, z)
                                            2 -> (v : x, y, z)
                                            _ -> undefined
                                    )
                           ) (0 :: Int, ([], [], []))
            ) [1 ..]

distance2 :: Point -> Point -> Int
distance2 (_, (ax, ay, az)) (_, (bx, by, bz)) =
    (ax - bx) * (ax - bx) + (ay - by) * (ay - by) + (az - bz) * (az - bz)

sortByDistance :: [Point] -> [(Point, Point)]
sortByDistance ps =
    sortBy (compare `on` uncurry distance2)
        [(a, b) | a@(i, _) <- init ps, b <- drop i ps]

aoc08p1 :: Int -> Int -> [Point] -> Int
aoc08p1 n m =
    product
    . take m
    . sortBy (flip compare)
    . map length
    . DS.toLists
    . foldr (uncurry DS.union . bimap fst fst) DS.empty
    . take n
    . sortByDistance

silentHead :: [a] -> a
silentHead = maybe undefined fst . uncons

aoc08p2 :: [Point] -> Int
aoc08p2 ps = snd $
    silentHead $ dropWhile (\(s, _) -> DS.values s < n || DS.sets s > 1) $
        scanl (\(s, _) ((a, (ax, _, _)), (b, (bx, _, _))) ->
                  (DS.union a b s, ax * bx)
              ) (DS.empty, 0) $ sortByDistance ps
    where n = length ps

