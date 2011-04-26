module Main where

import Data.List (sort)

-- brute force
combinations :: Int -> [Int] -> Int
combinations total coins = length . filter (\xs -> sum xs == total) . sequence $ [map (* c) [0 .. total `div` c] | c <- coins]  

-- smart
combinations' :: Int -> [Int] -> Int
combinations' total (coin:[]) 
  | total == 0 = 1
  | total `mod` coin == 0 = 1
  | otherwise = 0
combinations' total (coin:coins) = 
  let ds = [0 .. total `div` coin] 
  in sum $ map (\m -> combinations' (total - m * coin) coins) ds

main = do
    lss <- lines `fmap` getContents
    let cs = map (uncurry combinations' . parse) $ tail lss

    putStr $ unlines . map show $ cs

  where parse :: String -> (Int, [Int])
        parse s = let (t:ps) = words s in (read t, reverse . sort $ map read ps)
