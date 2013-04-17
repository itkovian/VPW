-- Example solution for the binaire_puzzel assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.IntMap as M
import           Data.List
import           Debug.Trace

-------------------------------------------------------
-- Verify that a given list of strings forms a correct
-- solution
verify :: [[Char]] -> Bool
verify ks =
    let ks' = transpose ks
    in    uniq ks
       && uniq ks'
       && grouping ks
       && grouping ks'
       && counting ks
       && counting ks'
  where grouping ss = and $ map (null . filter ((> 2) . length) . group) ss
        counting ss = and $ map ((\(zeros:ones:_) -> length zeros == length ones) . group . sort) ss
        uniq ss = (length $ nub ss) == length ss

-------------------------------------------------------
-- Solve a single case
solve :: IO Bool
solve = do
    s <- read `fmap` getLine
    ps <- sequence $ replicate s getLine
    return $ verify ps

-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    return $ trace ("cases = " ++ show n) ()
    s <- sequence $ replicate n solve

    putStr $ unlines $ map (\b -> if b then "juist" else "fout") s
