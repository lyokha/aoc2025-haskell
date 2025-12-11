module Aoc10 (readInput10, aoc10p1) where

import Data.Bits
import Data.Word
import Data.List
import Data.Bifunctor

zero :: Word16
zero = 0

readInput10 :: String -> [(Word16, [[Int]])]
readInput10 =
    map (maybe undefined
            (bimap
                (foldl (\a (i, v) ->
                           if v == '#' then setBit a i else a
                       ) zero . zip [0 :: Int ..] . takeWhile (/= ']') . drop 1
                )
                (map (read . (++ "]") . ('[' :) . drop 1 . init))
            ) . uncons . init . words
        ) . lines

aoc10p1 :: Word16 -> [[Int]] -> Int
aoc10p1 final buttons = go 0 [zero]
    where go depth lamps =
              let lamps' = nub $ concatMap clicks lamps
              in if final `elem` lamps'
                     then depth + 1
                     else go (depth + 1) lamps'
          clicks lamp = map (.^. lamp) buttons'
          buttons' = map (foldl setBit zero) buttons


