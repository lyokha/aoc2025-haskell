module Aoc04 (aoc04) where

import Data.Array.IArray

readGrid :: String -> Array Int (Array Int Char)
readGrid grid =
    let rows = lines grid
        len = length rows
    in listArray (1, len) $ map (\row -> listArray (1, length row) row) rows

adjacentCells :: Int -> (Int, Int) -> [(Int, Int)]
adjacentCells bound (x, y) =
    filter (\(a, b) -> a > 0 && a < (bound + 1) && b > 0 && b < (bound + 1))
        [(x - 1, y - 1), (x - 1, y), (x - 1, y + 1)
        ,(x, y - 1), (x, y + 1)
        ,(x + 1, y - 1), (x + 1, y), (x + 1, y + 1)
        ]

aoc04 :: String -> Integer
aoc04 grid = snd $
    foldr (\row (i, k) ->
              (i + 1
              ,k + snd $
                  foldr (\v (j, l) ->
                            (j + 1
                            ,if v == '@'
                                 then l + foldr (\(x, y) a -> if ar ! x ! y == '@'
                                                              then a + 1
                                                              else a
                                                ) 0 $ adjacentCells len (i, j)
                                 else l
                            )
                        ) (1, 0) row
              )
          ) (1, 0) ar
    where ar = readGrid grid
          len = fst $ bounds ar

