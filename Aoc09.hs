module Aoc09 (readInput09, aoc09p1, contour09, aoc09p2) where

import Data.List
import Data.Bifunctor
import Data.Function
import Data.IntMap qualified as M
import Data.IntMap (IntMap)

type Point = (Int, Int)
type Range = (Int, Int)

silentTail :: [a] -> [a]
silentTail = maybe undefined snd . uncons

readInput09 :: [String] -> [Point]
readInput09 = map $ bimap read (read . silentTail) . break (== ',')

area :: [Point] -> [Point] -> Int
area [(ax, a1y)] [(bx, b1y)] =
    (bx - ax + 1) * (abs (b1y - a1y) + 1)
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
    foldr (max . uncurry area) 0
    . (\ps -> [(a, b) | (i, a) <- zip [0 :: Int ..] ps, b <- drop i ps])
    . map shrink
    . groupBy ((==) `on` fst)
    . sort
    where shrink [] = undefined
          shrink a@[_] = a
          shrink (a : bs) = [a, last bs]

contour09 :: [Point] -> (IntMap Range, IntMap Range)
contour09 ps =
    bimap M.fromList M.fromList $
    foldr (\((ax, ay), (bx, by)) (xs, ys) ->
              if ax == bx
                  then ((ax, (min ay by, max ay by)) : xs, ys)
                  else (xs, (ay, (min ax bx, max ax bx)) : ys)
          ) ([], []) $ zip ps $ silentTail $ cycle ps

aoc09p2 :: (IntMap Range, IntMap Range) -> [Point] -> Int
aoc09p2 (bsx, bsy) =
    foldr (max . uncurry area) 0
    . (\ps -> [([a], [b])
              |(i, a@(ax, ay)) <- zip [0 :: Int ..] ps
              ,b@(bx, by) <- drop i ps
              ,let msy = (min ay by, max ay by)
              ,inBounds msy (ax, bx) bsx
              ,inBounds (ax, bx) msy bsy
              ]
      )
    . sort
    where inBounds (ay, by) (ax, bx) =
              all ((\(l, h) -> ay >= h || by <= l) . snd)
              . takeWhile ((< bx) . fst) . dropWhile ((<= ax) . fst)
              . M.assocs

