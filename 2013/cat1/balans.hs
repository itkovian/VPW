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
-- Solve a single case
solve :: IO (Int, Bool)
solve = do
    g <- read `fmap` getLine :: IO Int -- number of weights, can be ignored
    gs <- (sort . map read . words) `fmap` getLine -- weights
    c <- read `fmap` getLine :: IO Int -- case

    let ss = S.fromList [w + w' | w <- gs, w' <- gs, w' > w, w + w' <= c]

    return (c, S.member c ss)

-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    s <- replicateM n solve

    putStr $  unlines . map (\(c, b) -> show c ++ if b then " JA" else " NEEN") $ s

