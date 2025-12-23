module Aoc11 (readInput11, aoc11p1, aoc11p2) where

import Data.List
import Data.Map qualified as M
import Data.Map (Map, (!))
import Data.Bifunctor
import Control.Monad.ST
import Data.STRef

readInput11 :: String -> Map String [String]
readInput11 =
    M.fromList . map (maybe undefined (first init) . uncons . words) . lines

aoc11p1 :: String -> String -> Map String [String] -> Integer
aoc11p1 start end m = go [start]
    where go ps@(p : pso)
              | p == end = 1
              | p `elem` pso = 0
              | otherwise = sum $ map (go . (: ps)) $ m ! p
          go [] = undefined

aoc11p2 :: String -> String -> String -> Map String [String] -> Integer
aoc11p2 start end sentinel m = runST $ do
    memo <- newSTRef M.empty
    let go ps@(p : pso)
            | p == end = return 1
            | p `elem` pso || p == sentinel = return 0
            | otherwise =
                sum <$> mapM (\c -> do
                                  memoized <- readSTRef memo
                                  case c `M.lookup` memoized of
                                      Just c' -> return c'
                                      Nothing -> do
                                          nv <- go $ c : ps
                                          modifySTRef memo $ M.insert c nv
                                          return nv
                             ) (m ! p)
        go [] = undefined
    go [start]

