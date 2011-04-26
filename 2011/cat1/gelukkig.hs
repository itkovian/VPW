module Main where

import Data.List (elem)


happy :: [Int] -> Int -> String
happy is i 
  | i == 1 = "JA"
  | otherwise = let s = sq i
                in case s `elem` is of
                      True  -> "NEE"
                      False -> happy (s:is) s
  
sq :: Int -> Int
sq 0 = 0
sq i = r*r + sq q
  where q = i `div` 10
        r = i `mod` 10

main = do
    lss <- lines `fmap` getContents
    let cases = read $ head lss :: Int

    putStr $ unlines $ map (happy [] . read) $ tail lss 