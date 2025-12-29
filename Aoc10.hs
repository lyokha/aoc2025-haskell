{-# LANGUAGE CPP #-}

module Aoc10 (readInput10, aoc10p1, aoc10p2) where

import Data.Bits
import Data.Word
import Data.List
import Data.Bifunctor

#ifdef WITH_Z3
import Data.Maybe
import Z3.Monad
#endif

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
                               ) zero
                         . zip [0 ..] . takeWhile (/= ']') . drop 1
                        )
                        (map (read . (++ "]") . ('[' :) . drop 1 . init))
                    ) . uncons
                )
                (read . (++ "]") . ('[' :) . drop 1 . init)
            ) . unsnoc . words
        ) . lines
#if !MIN_VERSION_base(4,19,0)
    where unsnoc [] = Nothing
          unsnoc v = Just (init v, last v)
#endif

aoc10p1 :: Word16 -> [[Int]] -> Int
aoc10p1 final buttons = go 0 [zero]
    where go depth lamps =
              let lamps' = nub $ concatMap clicks lamps
              in if final `elem` lamps'
                     then depth + 1
                     else go (depth + 1) lamps'
          clicks lamp = map (.^. lamp) buttons'
          buttons' = map (foldl setBit zero) buttons

#ifdef WITH_Z3

silentTail :: [a] -> [a]
silentTail = maybe undefined snd . uncons

z3Script :: [Integer] -> [[Integer]] -> Z3 (Integer, [Integer])
z3Script final buttons = do
    zero' <- mkInteger 0
    final' <- mapM mkInteger final
    vars <- mapM (mkFreshIntVar . ('x' :) . show) [1 .. length buttons]
    constraints <- mapM (mkLe zero') vars
    eqs <- mapM (\(i, r) -> do
                    part <- mkAdd (foldl (\a (b, v) ->
                                             a ++ [v | b == (1 :: Integer)]
                                         ) [] $ zip (buttons' !! i) vars
                                  )
                    mkEq part r
                ) $ zip [0 ..] final'
    mapM_ optimizeAssert constraints
    mapM_ optimizeAssert eqs
    o <- mkAdd vars >>= optimizeMinimize
    Sat <- optimizeCheck []
    m <- optimizeGetModel
    o' <- optimizeGetLower o
    fromJust . uncons <$> mapM (fmap fromJust . evalInt m) (o' : vars)
    where buttons' = transpose $
              map (foldr ((\v a -> let (h, t) = splitAt v a
                                   in h ++ 1 : silentTail t
                          ) . fromIntegral
                         ) $ replicate (length final) 0
                  ) buttons

aoc10p2 :: [Int] -> [[Int]] -> IO (Integer, [Integer])
aoc10p2 final =
    evalZ3 . z3Script (map fromIntegral final) . map (map fromIntegral)

#else

aoc10p2 :: [Int] -> [[Int]] -> IO (Integer, [Integer])
aoc10p2 = const $ const $ return (0, [])

#endif

