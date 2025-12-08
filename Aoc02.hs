module Aoc02 (ranges02, filterEven02, aoc02) where

import Data.List.NonEmpty qualified as NE

ranges02 :: String -> [[((Integer, Integer), Int)]]
ranges02 =
    map ((\(l, r) ->
            case r of
                '-' : r' ->
                    let (lv, rv) = (read l, read r')
                    in map (\n ->
                               let nup = 10 ^ n - 1
                                   nbot = 10 ^ (n - 1)
                               in ((max lv nbot, min rv nup), n)
                           ) [length l .. length r']
                _ -> undefined
         ) . break (== '-')
        ) . words . map (\v -> if v == ',' then ' ' else v)

filterEven02 :: [[((Integer, Integer), Int)]] -> [[((Integer, Integer), Int)]]
filterEven02 = filter (not . null) . map (filter (even . snd))

generateInvalidIds :: Integer -> Integer -> Int -> Int -> [Integer]
generateInvalidIds _ _ 1 _ = []
generateInvalidIds l r n grpLen =
    map (read . take n . cycle . show) [takeHeadInteger l .. takeHeadInteger r]
    where takeHeadInteger = read @Integer . take grpLen . show

bigFactors :: Int -> [Int]
bigFactors n = map ((n `div`) . NE.head) . NE.group $ primeFactors n
    where primeFactors k
              | [] <- factors = [k]
              | fs@(h : _) <- factors = fs ++ primeFactors (k `div` h)
              where factors = take 1 $ filter ((0 ==) . (k `mod`)) [2 .. k - 1]

aoc02 :: Int -> [((Integer, Integer), Int)] -> [Integer]
aoc02 part =
    foldr (\((l, r), n) ->
              (++ (foldr (mergeUnique
                         . takeWhile (<= r) . dropWhile (< l)
                         . generateInvalidIds l r n
                         ) [] $ factors part n
                  )
              )
          ) []
    where factors 1 = pure . (`div` 2)
          factors _ = bigFactors
          -- mergeUnique is required for values like 1111111111 which can be
          -- made from combinations of different factors like 11111-11111
          -- (factor 5) or 11-11-11-11-11 (factor 2)
          mergeUnique [] ys = ys
          mergeUnique xs [] = xs
          mergeUnique xs'@(x : xs) ys'@(y : ys)
              | y == x = mergeUnique xs' ys
              | y < x = y : mergeUnique xs' ys
              | otherwise = x : mergeUnique xs ys'

