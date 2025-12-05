module Aoc05 (readInput05, mergeRanges05, aoc05p1, aoc05p2) where

import Data.Bifunctor
import Data.List

silentTail :: [a] -> [a]
silentTail = maybe undefined snd . uncons

readInput05 :: [String] -> ([(Integer, Integer)], [Integer])
readInput05 =
    bimap (map $ bimap read readWithPrefix . break ('-' ==))
          (map read . silentTail)
    . break ("" ==)
    where readWithPrefix [] = undefined
          readWithPrefix (_ : v) = read v

mergeRanges05 :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeRanges05 vs
    | (h : t) <- sort vs = (\(v, vs) -> vs ++ pure v) $
        foldl (\ca@(a@(l, r), vs) v@(vl, vr) ->
                  if vl > r
                      then (v, vs ++ pure a)
                      else if vr <= r
                               then ca
                               else ((l, vr), vs)
              ) (h, []) t
    | otherwise = []

aoc05p1 :: [(Integer, Integer)] -> [Integer] -> Int
aoc05p1 ranges values = fst $
    foldl (\(n, vs) (l, r) ->
              first ((n +) . length) $ break (> r) $ dropWhile (< l) vs
          ) (0, sort values) ranges

aoc05p2 :: [(Integer, Integer)] -> Integer
aoc05p2 = foldr (\(l, r) a -> a + (r - l) + 1) 0

