module Aoc03 (aoc03) where

aoc03 :: [String] -> Int
aoc03 = sum . map (read . snd
                   . foldl (\(n, lr) v ->
                               case lr of
                                   [l, r] ->
                                       if n > l
                                           then (v, n : pure v)
                                           else if v > r
                                                    then (v, l : pure v)
                                                    else (v, l : pure r)
                                   _ -> undefined
                           ) ('0', "00")
                  )

