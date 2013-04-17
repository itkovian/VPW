-- Example solution for the kalender assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import           Data.List

-------------------------------------------------------
-- find the first inde where we find p subsequent A's
-- in each of the kalendars
firstIndex :: [[Char]] -> Int -> Int -> Maybe Int
firstIndex ks p d =
    let ks' = group $ collapse ks
    in fi ks' 1
  where fi :: [[Char]] -> Int -> Maybe Int
        fi [] _ = Nothing
        fi (cs:css) i
          | (<=) p $ length $ filter (== 'V') cs = Just i
          | otherwise = fi css (i + (length cs))

-------------------------------------------------------
-- merge the calendar strings into a single string
collapse :: [[Char]] -> [Char]
collapse = foldl1 (\c k -> zipWith occupied c k)
  where occupied a b
          | a == 'B' || b == 'B' = 'B'
          | otherwise            = 'V'


-------------------------------------------------------
-- Solve a single case
solve :: IO String
solve = do
    k <- read `fmap` getLine
    d <- read `fmap` getLine
    p <- read `fmap` getLine

    ks <- replicateM k getLine

    return $ case firstIndex ks p d of
                Just i -> show i
                Nothing -> "X"

-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve

    putStr $ unlines s
