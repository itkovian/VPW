{-
 - (C) 2012 Andy Georges
 -
 - Oplossing voor de VPW 2012 opgave hexmap.
 -
 -}


module Main where

mapMask :: String -> String -> String -> String
mapMask _ [] _ = []
mapMask (f:fs) (m:ms) (b:bs) = 
    (case m of
        'F' -> f
        _ -> b) : mapMask fs ms bs


solve :: IO String
solve = do
    w:h:_ <- (map read . words) `fmap` getLine
    fore <- getFig h
    getLine
    mask <- getFig h
    getLine
    back <- getFig h
    let final = mapMask fore mask back
    return $ unlines . ((show w ++ " " ++ show h) :). (splitUp w) $ final
  where getFig h = fmap concat $ sequence $ replicate h getLine
        splitUp :: Int -> String -> [String]
        splitUp w [] = []
        splitUp w ss = let (h,ts) = splitAt w ss
                       in h : splitUp w ts

main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve
    mapM putStr s
