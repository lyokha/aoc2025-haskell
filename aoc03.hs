module Aoc03 (aoc03p1, aoc03p2) where

-- solution with a one digit accumulator
aoc03p1 :: [String] -> Integer
aoc03p1 = sum . map (read . snd
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

-- solution with recursion and a sliding window
aoc03p2 :: [String] -> Integer
aoc03p2 = sum . map (read . reverse . go 12 0 "")
    where go 0 _ val _ = val
          go n start val s =
              let (ndigit, nstart) =
                      snd $ foldl (\(i, nv'@(nv, _)) v ->
                                      let ni = i + 1
                                      in if v > nv
                                             then (ni, (v, ni))
                                             else (ni, nv')
                                  ) (0, ('0', 0)) $
                                      take (length s - start - (n - 1)) $
                                          drop start s
              in go (n - 1) (start + nstart) (ndigit : val) s

