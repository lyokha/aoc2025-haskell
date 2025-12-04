module Aoc04 (aoc04p1, aoc04p2) where

import Data.Array.IArray
import Data.IntMap qualified as M
import Data.IntMap (IntMap)
import Data.Foldable
import Data.Bifunctor
import Data.List

readGrid :: String -> Array Int (Array Int Char)
readGrid input =
    let rows = lines input
        len = length rows
    in listArray (1, len) $ map (\row -> listArray (1, length row) row) rows

adjacentCells :: Int -> (Int, Int) -> [(Int, Int)]
adjacentCells bound (x, y) =
    filter (\(a, b) -> a > 0 && a <= bound && b > 0 && b <= bound)
        [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)
        ,(x, y - 1), (x, y + 1)
        ,(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
        ]

step :: Array Int (Array Int Char) -> (Integer, IntMap [Int])
step grid = snd $
    foldl (\(i, (k, is)) row ->
              (i + 1
              ,let next = snd $
                       foldl (\(j, (l, a)) v ->
                                 let accessible = accessibleRoll v i j
                                 in (j + 1
                                    ,(l + if accessible then 1 else 0
                                     ,if accessible
                                          then M.insertWith (++) i [j] a
                                          else a
                                     )
                                    )
                             ) (1, (0, M.empty)) row
               in bimap (k +) (is `M.union`) next
              )
          ) (1, (0, M.empty)) grid
    where len = snd $ bounds grid
          countAdjacentRolls i j =
              foldr (\(x, y) -> (+ if grid ! x ! y == '@' then 1 else 0)) 0 $
                  adjacentCells len (i, j)
          accessibleRoll '@' = ((< 4) .) . countAdjacentRolls
          accessibleRoll _ = const $ const False

aoc04p1 :: String -> Integer
aoc04p1 = fst . step . readGrid

silentHead :: [a] -> a
silentHead = maybe undefined fst . uncons

aoc04p2 :: String -> Integer
aoc04p2 input = fst $ silentHead $ dropWhile (not . fst . snd) $
    scanl (\(n, (_, grid')) _ ->
              let (k, is) = step grid'
              in (n + k
                 ,(k == 0
                  ,listArray (1, len) $ reverse $ snd $
                      foldl (\(i, a) row ->
                                (i + 1
                                ,(: a) $ case M.lookup i is of
                                             Just vs -> updateRow row vs
                                             Nothing -> row
                                )
                            ) (1, []) grid'
                  )
                 )
          ) (0, (False, grid)) [0 ..]
    where grid = readGrid input
          len = snd $ bounds grid
          updateRow row vs = listArray (1, len) $ reverse $
              foldl (\a (j, v) -> (if j `elem` vs then 'x' else v) : a)
                  [] (zip [1 ..] $ toList row)

