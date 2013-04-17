-- Example solution for the ballensorteerder assignment VPW 2013
-- (C) 2013 Andy Georges
--
-------------------------------------------------------
module Main where

import           Control.Monad
import qualified Data.Set as S
import           Data.List
import           Debug.Trace


-------------------------------------------------------
-- Formula for the increase diameter given the increase in volume
diameter :: Int -> Double
diameter v = 2 * (3.0 / 4.0 * (fromIntegral v) / pi) ** (1 / 3.0)

-------------------------------------------------------
-- Solve a single case
solve :: IO [String]
solve = do
    name <- getLine
    d <- read `fmap` getLine
    n <- read `fmap` getLine
    volumes <- sequence $ replicate n $ read `fmap` getLine

    let matches = map (\v -> if abs (diameter v - d) < 0.5 then name else "geen " ++ name) volumes

    return matches



-------------------------------------------------------
-- Main, duh.
main = do
    n <- read `fmap` getLine :: IO Int
    s <- sequence $ replicate n solve

    mapM_ putStr $ map unlines $ s

