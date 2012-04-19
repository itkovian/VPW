{-
 - (C) 2012 Andy Georges
 -
 - Oplossing voor de VPW 2012 opgave verkiezingen.
 -
 -}

module Main where

import Data.List (intercalate)

votes :: Int -> Int -> [Int] -> Int -> [Int] -> [Int]
votes z total vs d zs
  | z < (length $ filter (\v -> fromIntegral v >= margin) vs) = zs
  | otherwise = 
      let as = map (\v -> if fromIntegral v >= margin
                             then (+1)
                             else id) vs
          zAssigned = sum $ zipWith ($) as $ repeat 0
          zs' = zipWith ($) as zs
      in votes (z - zAssigned) total vs (d+1) zs'
  where margin = fromIntegral total / fromIntegral d


solve :: IO String
solve = do
    z <- read `fmap` getLine
    p <- read `fmap` getLine
    vs <- (map read . words) `fmap` getLine
    let vs' = votes z (sum vs) vs 2 (replicate p 0)
    return $ intercalate " " $ map show vs'

main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve
    mapM putStrLn s 
