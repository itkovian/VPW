import Data.Maybe
import Data.List
import System.IO
import Control.Applicative


data Perfect = P Int Int | None

instance Show Perfect where
  show (P p v) = show p ++ " " ++ show v
  show None = "GEEN"


divisors :: Int -> [Int]
divisors m =
  let upperBound = round . sqrt . fromIntegral $ m
  in nub $ concat $ [[m `div` d, d] | d <- [1..upperBound], m `mod` d == 0]


isVPerfect :: Int -> Maybe Perfect
isVPerfect n =
  let s = sum $ divisors n
  in if s `mod` n == 0 then Just (P n (s `div` n))
     else Nothing


perfects :: [Int] -> Perfect
perfects (m:n:_) = 
  let ps = catMaybes $ map isVPerfect [m .. n]
  in case ps of
    [] -> None
    (h:_) -> h


main = do
  t <- read <$> getLine :: IO Int
  lss <- lines <$> getContents

  let ps = map (perfects . map read . words) $ lss

  putStr $ unlines $ map show ps
