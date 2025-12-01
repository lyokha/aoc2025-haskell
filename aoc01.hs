module Aoc01 (aoc01) where

readInc :: String -> Integer
readInc ('R' : v) = read v
readInc ('L' : v) = negate $ readInc $ 'R' : v
readInc _ = undefined

mInc :: Integer -> Integer -> Integer
mInc = ((`mod` 100) .) . (+)

nInc :: Integer -> Integer -> Integer
nInc cur v
    | v' <= 0 = abs ((v' - 1) `div` 100) - if cur == 0 then 1 else 0
    | otherwise = v' `div` 100
    where v' = v + cur

aoc01 :: Integer -> [String] -> (Integer, (Integer, Integer))
aoc01 start =
    foldl (\(cur, (v1, v2)) op ->
              let inc = readInc op
                  next = mInc cur inc
              in (next, (v1 + if next == 0 then 1 else 0, v2 + nInc cur inc))
          ) (start, (0, 0))

