module Aoc02 (ranges02, ranges02a) where

ranges02 :: String -> [((Integer, Integer), [Int])]
ranges02 =
    filter (not . null . snd)
    . map ((\(l, r) -> case r of
                           '-' : r' -> ((read l, read r')
                                       ,filter even [length l .. length r']
                                       )
                           _ -> undefined
           ) . break (== '-')
          ) . words . map (\v -> if v == ',' then ' ' else v)

ranges02a :: ((Integer, Integer), [Int]) -> [Integer]
ranges02a ((l, r), ns) =
    let range n = let n' = n `div` 2
                  in map (\v -> let v' = show v in read $ v' ++ v')
                         [read @Integer ('1' : replicate (n' - 1) '0')
                         .. read (replicate n' '9')
                         ]
    in foldr (\n -> (++ (takeWhile (<= r) . dropWhile (< l) $ range n))) [] ns

