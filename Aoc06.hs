module Aoc06 (aoc06p1, aoc06p2) where

import Data.List
import Data.Maybe
import Data.Bifunctor

silentHead :: [a] -> a
silentHead = maybe undefined fst . uncons

silentTail :: [a] -> [a]
silentTail = maybe undefined snd . uncons

opChar :: Char -> Bool
opChar '*' = True
opChar '+' = True
opChar _ = False

opRow :: (String, [String]) -> Integer
opRow = uncurry foldr1 . bimap op (map read)
    where op "*" = (*)
          op "+" = (+)
          op _ = undefined

aoc06p1 :: String -> Integer
aoc06p1 =
    sum
    . map (opRow . fromJust . uncons . reverse)
    . transpose
    . map words
    . lines

aoc06p2 :: String -> Integer
aoc06p2 =
    sum
    . map (opRow
           . (\(h, t) -> (pure $ silentHead h, map reverse $ silentTail h : t))
           . fromJust . uncons
          )
    . groupBy (const $ not . opChar . silentHead)
    . map reverse
    . filter (any (/= ' '))
    . transpose
    . lines

