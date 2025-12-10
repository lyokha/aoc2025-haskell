module Aoc09 (readInput09, aoc09p1, aoc09p2) where

import Data.List
import Data.Bifunctor
import Data.Function

type Point = (Int, Int)

silentTail :: [a] -> [a]
silentTail = maybe undefined snd . uncons

readInput09 :: [String] -> [Point]
readInput09 = map $ bimap read (read . silentTail) . break (== ',')

area :: [Point] -> [Point] -> Int
area [(ax, a1y)] [(bx, b1y)] =
    (bx - ax + 1) * (b1y - a1y + 1)
area [(ax, a1y), (_, a2y)] [(bx, b1y)] = max
    ((bx - ax + 1) * (abs (b1y - a1y) + 1))
    ((bx - ax + 1) * (abs (b1y - a2y) + 1))
area [(ax, a1y)] [(bx, b1y), (_, b2y)] = max
    ((bx - ax + 1) * (abs (b1y - a1y) + 1))
    ((bx - ax + 1) * (abs (b2y - a1y) + 1))
area [(ax, a1y), (_, a2y)] [(bx, b1y), (_, b2y)] = maximum
    [(bx - ax + 1) * (abs (b1y - a1y) + 1)
    ,(bx - ax + 1) * (abs (b2y - a1y) + 1)
    ,(bx - ax + 1) * (abs (b1y - a2y) + 1)
    ,(bx - ax + 1) * (abs (b2y - a2y) + 1)
    ]
area _ _ = undefined

aoc09p1 :: [Point] -> Int
aoc09p1 =
    foldr (\(ps1, ps2) a -> let n = area ps1 ps2 in max a n) 0
    . (\ps -> [(a, b) | (i, a) <- zip [0 :: Int ..] ps, b <- drop i ps])
    . map shrink
    . groupBy ((==) `on` fst)
    . sort
    where shrink [] = undefined
          shrink a@[_] = a
          shrink (a : bs) = [a, last bs]

minmax :: Ord a => a -> a -> (a, a)
minmax a b | a < b = (a, b)
           | otherwise = (b, a)

-- still gives a wrong answer
aoc09p2 :: [Point] -> [Int]
aoc09p2 ps =
    map (\((x1, y1), (x, y), (x2, y2)) ->
            let ((xmin, xmax), (ymin, ymax)) =
                    if x == x1
                        then (minmax x x2, minmax y y1)
                        else (minmax x x1, minmax y y2)
                xs = takeWhile ((<= xmax) . fst) $
                    dropWhile ((< xmin) . fst) pss
                psc = filter (\(_, py) -> py >= ymin && py <= ymax) xs
            in foldr (\(px, py) ->
                         max ((abs (px - x) + 1) * (abs (py - y) + 1))
                     ) 0 psc
        ) $ zip3 ps psn $ silentTail psn
    where psn = silentTail $ cycle ps
          pss = sort ps

