{-# LANGUAGE BangPatterns #-}
{-
 - (C) 2012 Andy Georges
 -
 - Oplossing voor de VPW opgave cat 3 Morse.
 -
 -}

module Main where

import Control.Monad (liftM) 

recurse :: (Integer, Integer) -> Int -> Integer
recurse (!p,!k) 0 = p + k
recurse (!p, !k) n = 
  let p' = 3*k + p
      k' = k + p
  in recurse (p', k') (n-1)


solve :: IO String
solve = do
    (pattern,n) <- ((\(p:k:_) -> (p, read k)) . words) `fmap` getLine
    let p = length $ filter ( (==) '.') pattern
        k = length $ filter ( (==) '-') pattern
    return $ show $ recurse (fromIntegral p, fromIntegral k) n


main = do 
    n <- read `fmap` getLine
    ss <- sequence $ replicate n solve
    mapM putStrLn ss
