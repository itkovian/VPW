{-
 - (C) 2012 Andy Georges
 -
 - Oplossing voor de VPW 2012 opgave gevangen.
 -
 -}


module Main where


vangen :: Int -> [Int] -> ([Int], String) -> ([Int], String)
vangen _ [] (vs, s) = (reverse vs, "alles gevangen")
vangen l cs (vs, s) = 
    case vangen' 0 $ zip (concat $ replicate (length cs) [1..l]) (concat $ replicate l cs) of
      Nothing    -> (reverse vs, "er kunnen geen kaarten meer gevangen worden")
      Just (i,c) -> let i' = i `mod` (length cs)
                        hs = take i' cs
                        ts = drop (i'+1) cs
                    in vangen l (ts ++ hs) (c:vs, s) 

vangen' :: Int -> [(Int, Int)] -> Maybe (Int, Int)
vangen' p [] = Nothing
vangen' p ((i,c):cs)
  | i == c = Just (p,c)
  | otherwise = vangen' (p+1) cs



solve :: IO [String]
solve = do  
    (l:cs) <- (map read . words) `fmap` getLine
    let (is, s) = vangen l cs ([],"")
    return $ (map (\i -> show i ++ " gevangen") is) ++ [s]


main = do
    n <- read `fmap` getLine
    s <- sequence $ replicate n solve
    mapM putStr $ map unlines s

