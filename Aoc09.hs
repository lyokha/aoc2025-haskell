module Aoc09 (readInput09, buildPoints09, aoc09p1, aoc09p2, aoc09p1a) where

import Data.List
import Data.IntMap qualified as M
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

aoc09p1a ps =
    take 500 $ filter even $
    map (\(i, (_, y1), (x1, y2), (x2, _)) ->
            if any (\(x, y) -> x > min x1 x2 && x < max x1 x2 && y > min y1 y2 && y < max y1 y2) ps
                then 0
                else (1 + abs (x1 - x2)) * (1 + abs (y1 - y2))
        ) $ zip4 [0 :: Int ..] ps' (tail ps') (tail (tail ps'))
    where ps' = cycle ps

type Points = [(Int, (Point, ([(Int, Point)], [(Int, Point)])))]

buildPoints09 :: [Point] -> Points
buildPoints09 ps = M.toList $
    M.foldlWithKey (\a i (py, psy'') ->
                       M.insert i (py, (snd (psx' M.! i), psy'')) a
                   ) M.empty $ build psy'
    where pss = zip [1 :: Int ..] $ sort ps
          psx = groupBy ((==) `on` (fst . snd)) pss
          psx' = build psx
          psy = sortBy (compare `on` (snd . snd)) pss
          psy' = groupBy ((==) `on` (snd . snd)) psy
          build =
              M.fromListWith (\(p, ps') (_, ps'') -> (p, ps' ++ ps''))
              . map (\((i, p), ps') -> (i, (p, ps')))
              . concatMap (\ps' -> [(a, b) | a <- ps', let b = delete a ps'])

aoc09p2 :: [Point] -> (Int, (Int, (Point, ([(Int, Point)], [(Int, Point)]))))
aoc09p2 = foldr (\v a@(m, _) -> let n = maxArea v in if n > m then (n, v) else a) (0, (0, ((0, 0), ([], [])))) . buildPoints09
    where maxArea (_, (_, ([], []))) = 1
          maxArea (_, ((_, y), (px, []))) =
              maximum $ map ((1 +) . abs . (`subtract` y) . snd . snd) px
          maxArea (_, ((x, _), ([], py))) =
              maximum $ map ((1 +) . abs . (`subtract` x) . fst . snd) py
          maxArea (_, ((x, y), (px, py))) =
              maximum (map ((1 +) . abs . (`subtract` y) . snd . snd) px)
              *
              maximum (map ((1 +) . abs . (`subtract` x) . fst . snd) py)

