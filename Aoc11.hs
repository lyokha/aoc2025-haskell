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
aoc11p1 start end m = go start [start]
    where go n ps@(p : pso)
              | p == end = 1
              | p `elem` pso = 0
              | otherwise = sum $ map (\c -> go c (c : ps)) (m ! n)
          go _ [] = undefined

aoc11p2 :: String -> String -> Map String [String] -> Integer
aoc11p2 start end m = runST $ do
    memo <- newSTRef M.empty
    go start [start] memo
    where go n ps@(p : pso) memo
              | p == end = return 1
              | p `elem` pso = return 0
              | otherwise =
                  sum <$> mapM (\c -> do
                                    memoized <- readSTRef memo
                                    case c `M.lookup` memoized of
                                        Just c' -> return c'
                                        Nothing -> do
                                            nv <- go c (c : ps) memo
                                            modifySTRef memo $ M.insert c nv
                                            return nv
                            ) (m ! n)
          go _ [] _ = undefined

