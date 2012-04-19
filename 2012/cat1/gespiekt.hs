{-
 - (C) 2012 Andy Georges
 -
 - Oplossing voor de VPW 2012 opgave gespiekt.
 -
 -}

module Main where

import Data.List (sort)

cheaters :: [Int] -> String
cheaters [] = "spieken kon niet"
cheaters (_:[]) = "spieken kon niet"
cheaters pss@(p:ps) = 
    let zs = zip pss [1..]
        ds = zipWith (\(p1, n) p2 -> (abs (p1 - p2), n)) zs ps
        ss = sort ds
    in case head ss of
          (0, n) -> show n ++ " en " ++ show (n+1) ++ " zijn zwaar verdacht"
          (_, n) -> show n ++ " en " ++ show (n+1) ++ " zijn verdacht"

solve :: IO String
solve = do
    (_:ps) <- (map read . words) `fmap` getLine
    return $ cheaters ps


main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve
    mapM putStrLn s
