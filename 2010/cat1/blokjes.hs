import System.IO
import Control.Monad


blocks :: Int -> Int
blocks n = foldr1 (+) $ zipWith3 (\x y z -> x*y*z) [1..n] [1..n] [1..n]


main = do
  t <- read `liftM` getLine :: IO Int
  lss <- lines `liftM` getContents

  let cubeCounts = map (read :: String -> Int) lss
      blockCounts = map blocks cubeCounts

  putStr $ unlines $ map show blockCounts
  

