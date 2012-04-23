import Data.List
import System.IO
import Control.Applicative

splitOn :: String -> String -> [String]
splitOn sep ss = splitOn' ss sep []

splitOn' :: String -> String -> String -> [String]
splitOn' [] _ acc = [reverse acc]
splitOn' sss@(s:ss) sep acc = 
  case stripPrefix sep sss of
    Just suffix -> (reverse acc):splitOn' suffix sep []
    Nothing -> splitOn' ss sep (s:acc)


mkGuess :: String -> ([Int], Int, Int)
mkGuess s = 
  let (guess:ss:_) = splitOn ":" s
      (b:w:_) = splitOn "," ss
  in (map read $ map (\c -> [c]) guess, read b, read w)



genCodes :: Int -> Int -> [[Int]] -> [[Int]]
genCodes 0 k css  = css
genCodes n k css = genCodes (n-1) k [ c:cs | c <- [1..k], cs <- css]


getW :: [Int] -> [Int] -> Int -> Int
getW _ [] w = w
getW [] _ w = w
getW (c:cs) (g:gs) w
  | c == g = getW cs gs (w+1)
  | c < g = getW cs (g:gs) w
  | c > g = getW (c:cs) gs w


canFit :: [Int] -> ([Int], Int, Int) -> Bool
canFit code (guess, b, w) = 
  let same = zipWith (==) code guess
      different = zipWith (/=) code guess
      differentCode = sort $ map fst $ filter snd $ zip code different
      differentGuess = sort $ map fst $ filter snd $ zip guess different
      w' = getW differentCode differentGuess 0
  in length (filter ((==) True) same) == b && w' == w


master :: (Int, Int, Int) -> [([Int], Int, Int)] -> [String]
master (n, k, g) gs = 
  let codes = filter (\guess -> and $ map (canFit guess) gs) $ genCodes n k [[]]
  in map (concat . map show) codes 



process :: [String] -> [[String]]
process [] = []
process (config:guesses:css) = 
  let (n:k:g:_) = map read $ words config
      gss = map mkGuess $ words guesses
  in master (n, k, g) gss : process css



main = do
  t <- read <$> getLine :: IO Int
  lss <- lines <$> getContents

  let gss = process lss

  putStr $ unlines $ map (\codes -> concat . intersperse " " $ (show $ length codes):codes) gss
