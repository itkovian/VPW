module Main where

import Data.List (concatMap, group, groupBy, lookup, nub, sort, transpose)
import Data.Maybe (fromJust)


busroddels :: Int -> [[Int]] -> Maybe Int
busroddels count ls = 
  let rs = [[i] | i <- [1..count]] -- initial gossips
      stopcounts = map head ls   -- number of stops for each bus driver
      stops = transpose $ map (concat . repeat . tail) ls -- map (take (min ((*) 8 $ foldl1 lcm stopcounts) 1440) . concat . repeat) ls -- maximal number of rounds each driver has to do 
      rss = scanl exchange rs $ stops 
  in case break (count <=) $ map (foldl1 min . map length) $ take 1440 rss of
        (_, []) -> Nothing
        (ms, _) -> Just (length ms - 1)

exchange :: [[Int]] -> [Int] -> [[Int]]
exchange roddels stops = 
  let gs = groupBy (\(s1, _) (s2, _) -> s1 == s2) . sort $ zip stops roddels
      gids = map (head . map fst) gs
      rss = map ( nub . concatMap snd ) gs
  in map (\s -> Data.Maybe.fromJust $ lookup s $ zip gids rss) stops


main = do
    lss <- lines `fmap` getContents
    let cases = read $ head lss :: Int
        minutes = bsr $ tail lss

    putStr $ unlines $ map (\m -> case m of 
                                      Just (-1) -> "0"
                                      Just x -> show x
                                      Nothing -> "NOOIT"
                             ) minutes 

  where bsr :: [String] -> [Maybe Int]
        bsr [] = []
        bsr (c:cs) = 
          let count = read c :: Int
              ls = map (map (read :: String -> Int) . words) $ take count cs
              rs = drop count cs
          in busroddels count ls : bsr rs
