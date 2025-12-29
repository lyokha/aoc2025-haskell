module Aoc10 (readInput10, aoc10p1, aoc10p2) where

import Data.Bits
import Data.Word
import Data.List
import Data.Bifunctor
import Numeric.LinearAlgebra

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

silentTail :: [a] -> [a]
silentTail = maybe undefined snd . uncons

-- yields negative values in some cases
aoc10p2 :: [Int] -> [[Int]] -> [Int]
aoc10p2 final buttons =
    let a = len >< length buttons $
                concatMap (map $ fromIntegral @Int @Double) $ transpose buttons'
        b = len >< 1 $ map fromIntegral final
    in map round $ concat $ toLists $ a <\> b
    where len = length final
          buttons' = map (foldr (\v a -> let (h, t) = splitAt v a
                                         in h ++ 1 : silentTail t
                                ) $ replicate (length final) 0
                         ) buttons

