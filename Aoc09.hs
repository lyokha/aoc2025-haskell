module Aoc09 (readInput09, aoc09p1, yBounds09, aoc09p2) where

import Data.List
import Data.Bifunctor
import Data.Function
import Data.IntMap qualified as M
import Data.IntMap ((!))

type Point = (Int, Int)

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

yBounds09 :: [Point] -> M.IntMap [Int]
yBounds09 ps = foldr update psx psy
    where psy = build $ sort $ map (\(x, y) -> (y, x)) ps
          psx = M.fromList $ build $ sort ps
          build = map pairs . groupBy ((==) `on` fst)
          pairs vs@((x, _) : _) = (x ,) $ map snd vs
          pairs [] = undefined
          update (y, [x1, x2]) =
              M.mapWithKey (\x ys -> if x < x1 || x > x2
                                         then ys
                                         else let ysn = y : ys
                                              in [minimum ysn, maximum ysn]
                           )
          update _ = undefined

-- still gives a wrong answer
aoc09p2 :: M.IntMap [Int] -> [Point] -> Int
aoc09p2 bsy =
    foldr (max . uncurry area) 0
    . (\ps -> [([a], [b]) | (i, a@(ax, ay)) <- zip [0 :: Int ..] ps
              ,b@(bx, by) <- drop i ps
              ,inBounds by $ bsy ! ax
              ,inBounds ay $ bsy ! bx
              ]
      )
    . sort
    where inBounds y [y1, y2] = y >= y1 && y <= y2
          inBounds _ _ = undefined

