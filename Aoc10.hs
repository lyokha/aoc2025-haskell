module Aoc10 (readInput10, aoc10p1, aoc10p2) where

import Data.Bits
import Data.Word
import Data.List
import Data.Bifunctor

zero :: Word16
zero = 0

readInput10 :: String -> [((Word16, [[Int]]), [Int])]
readInput10 =
    map (maybe undefined
            (bimap
                (maybe undefined
                    (bimap
                        (foldl (\a (i, v) ->
                                   if v == '#' then setBit a i else a
                               ) zero . zip [0 :: Int ..] . takeWhile (/= ']')
                                 . drop 1
                        )
                        (map (read . (++ "]") . ('[' :) . drop 1 . init))
                    ) . uncons
                )
                (read . (++ "]") . ('[' :) . drop 1 . init)
            ) . unsnoc . words
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

aoc10p2 :: [Int] -> [[Int]] -> Int
aoc10p2 final buttons = go 0 [replicate len 0]
    where go depth jolts =
              let jolts' = nub $ concatMap clicks jolts
              in if final `elem` jolts'
                     then depth + 1
                     else go (depth + 1) jolts'
          clicks jolt = filter (all (uncurry (>=)) . zip final) $
              map (zipWith (+) jolt) buttons'
          buttons' = map (take len . (++ repeat 0)) buttons
          len = length final

