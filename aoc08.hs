module Aoc08 (readInput08, sortByDistance07) where

import Data.List

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

sortByDistance07 :: [Point] ->  [(Point, Point)]
sortByDistance07 ps = sortBy (\((_, (ax, ay, az)), (_, (bx, by, bz))) ((_, (cx, cy, cz)), (_, (dx, dy, dz))) ->
    compare ((ax - bx) * (ax - bx) + (ay - by) * (ay - by) + (az - bz) * (az - bz))
            ((cx - dx) * (cx - dx) + (cy - dy) * (cy - dy) + (cz - dz) * (cz - dz)))
    [(a, b) | a@(i, _) <- init ps, b <- drop i ps]

