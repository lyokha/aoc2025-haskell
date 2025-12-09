module Aoc09 (readInput09, aoc09p1) where

import Data.List
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty((:|)))
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
    . NE.groupBy ((==) `on` fst)
    . sort
    where shrink (a :| []) = [a]
          shrink (a :| bs) = [a, last bs]

