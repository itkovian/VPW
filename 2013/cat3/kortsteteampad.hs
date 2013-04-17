
-- Example solution for the balans assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.Set as S
import           Data.List
import           Debug.Trace

-------------------------------------------------------
-- shortest team path
shortest :: [(Int, Int)] -> Int
shortest ps =
    foldl1 min $ map combine $ [(p1, p2) | p1 <- ps, p2 <- ps, p1 /= p2]


combine :: ((Int, Int), (Int, Int)) -> Int
combine ((x1, y1), (x2, y2)) = (max x1 x2) + (max y1 y2)


-------------------------------------------------------
-- Solve a single case
solve :: IO Int
solve = do
    p <- read `fmap` getLine
    ps <- sequence $ replicate p $ ((\(x:y:_) -> (read x, read y)) . words) `fmap` getLine

    return $ shortest ps


-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    s <- sequence $ replicate n solve

    putStr $ unlines $ map show s
